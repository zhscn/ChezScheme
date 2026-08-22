# Asynchronous Fibers Design

## Scope

This document specifies a fiber-based asynchronous execution facility for
Chez Scheme. The facility combines delimited continuations with a libuv event
loop so that Scheme code can suspend at asynchronous operations without
blocking the scheduler's operating-system thread.

The public bindings belong to separate libraries rooted at
`(chezscheme async)`. [ASYNC_API.md](ASYNC_API.md) specifies their procedure
signatures and result formats. Existing synchronous ports, threads, engines,
and the bindings of `(chezscheme)` retain their current contracts.

The design supports:

- lightweight Scheme tasks represented by delimited continuations;
- cooperative scheduling with bounded fairness;
- structured task lifetime, joining, cancellation, and exception propagation;
- hierarchical cancellation contexts with deadlines and reasons;
- asynchronous timers, networking, name resolution, and file operations;
- composable wait operations, channels, and fiber-aware mutexes;
- an optional scheduler group spanning multiple operating-system threads; and
- optional timed preemption built on Chez Scheme engines.

Transparent suspension of every existing port operation is outside the core
contract. Async I/O uses handles owned by the async library. Integration with
ordinary ports requires an explicit adapter or a separate change to the Chez
Scheme port implementation.

## Terminology

A **task** is a logical Scheme computation managed by a scheduler. A running
task consists of an entry thunk or a one-shot resumption of a suspended
delimited continuation.

A **scheduler** owns a libuv loop, runnable queues, and completion queues. It
uses the private continuation-prompt tag of its scheduler group and executes
on at most one operating-system thread at a time.

A **scheduler group** is a set of schedulers that may exchange migratable
tasks. The group owns task identity, the private prompt tag, and the stealable
ready queue. Each member has its own operating-system thread and libuv loop.

An **operation** represents an action that can either complete immediately or
suspend its task until an external condition becomes true. I/O requests,
timers, channel sends and receives, and task joins are operations.

A **resumption** is a one-shot wrapper around a captured task continuation.
Exactly one completion, cancellation, or failure path may claim it.

A **task group** owns a set of child tasks and provides a structured lifetime
boundary for them.

A **cancellation context** is a thread-safe node in a propagation tree. It
owns a stable first cancellation reason and a completion operation used to
interrupt asynchronous waits.

## Library organization

The library boundaries are:

```scheme
(chezscheme async)
(chezscheme async context)
(chezscheme async operations)
(chezscheme async channels)
(chezscheme async sync)
(chezscheme async syntax)

(chezscheme async io errors)
(chezscheme async io stream)
(chezscheme async io dns)
(chezscheme async io udp)
(chezscheme async io fs)
(chezscheme async io poll)
(chezscheme async io process)
(chezscheme async io signal)
(chezscheme async io watch)
(chezscheme async io tty)
(chezscheme async io system)
```

Applications import the individual I/O domain libraries they use. The
scheduler, operation, and channel libraries do not initialize libuv when no
I/O operation is used. The kernel statically links vendored libuv and a small
C shim; libuv objects and ABI details are not exposed as Scheme objects.

Representative public forms are:

```scheme
(run-async thunk option ...)
(spawn-task thunk option ...)
(task? object)
(task-state task)
(task-join task)
(task-join-operation task)
(task-cancel! task)
(task-context task)
(task-yield)
(async-sleep seconds)

(perform-operation operation)
(choice-operation operation ...)
(wrap-operation operation procedure)

(make-async-context [parent])
(async-context-cancel! context [reason])
(async-context-done-operation context)
(call-with-async-timeout seconds thunk)

(make-channel [capacity])
(channel-put-operation channel value)
(channel-get-operation channel)
(channel-receive-operation channel)
(channel-close! channel [reason])
(channel-put channel value)
(channel-get channel)
(channel-receive channel)

(async body ...)
(go body ...)
(await task)
(select clause ...)
(with-timeout seconds body ...)
(with-cancel-scope (cancel!) body ...)
(channel-for (value channel) body ...)
```

## Scheduler ownership

Each scheduler owns:

- a reference to its scheduler group's private continuation-prompt tag;
- a current-turn run queue;
- a next-turn run queue;
- a thread-safe remote-submission queue;
- a queue of libuv completions;
- a local task registry used for scheduling and resource ownership;
- a libuv loop and its wakeup handle;
- the operating-system thread currently running it, if any; and
- monotonic counters for turns, task executions, suspensions, and wakeups.

Each `run-async` scheduler group receives a fresh tag. The tag is never the
default continuation-prompt tag and is not exported. User `reset`, `shift`,
and tagged control operations therefore cannot intercept scheduler
suspension, while a migratable task can resume under another scheduler in the
same group.

Only the owning operating-system thread may:

- call `uv_run` on the scheduler's loop;
- create, modify, or close handles belonging to that loop;
- move tasks between the scheduler's local queues; or
- invoke Scheme resumptions belonging to that scheduler.

Remote threads submit commands to the remote-submission queue and call
`uv_async_send`. A libuv worker or arbitrary foreign thread never invokes a
Scheme continuation directly.

## Task representation and states

A task contains at least:

```text
id
name
state
entry-or-resumption
scheduler
affinity
dynamic-state
exception-state
parent-group
context
child-group
result-values
failure-condition
join-waiters
cancellation-state
current-wait
```

The task state machine is:

```text
created -> ready -> running -> waiting -> ready
                     |           |
                     |           +-------> canceled
                     +-------------------> completed
                     +-------------------> failed
                     +-------------------> canceled
```

`completed`, `failed`, and `canceled` are terminal states. A terminal task is
never placed on a run queue. Its result remains available while the task or a
join waiter is reachable.

Task results preserve multiple values. Joining a completed task produces
those values. Joining a failed task raises its stored condition in the joining
task. Joining a canceled task raises the async cancellation condition.

Joining the current task is an error. Multiple tasks may join the same task.

## Structured task lifetime

`run-async` establishes a root task group. `spawn-task` creates a child in the
current task group unless an explicit group is supplied.

Every task owns a cancellation context. The root task receives an independent
root context. A spawned task receives a child of its explicit context, its
explicit task group's context, or the dynamically current context. This
separates task-local cancellation ownership from caller-supplied shared
contexts: canceling one task never cancels a shared parent context.

A task group obeys the following rules:

- normal group exit waits for all children;
- failure of the group body requests cancellation of unfinished children;
- cancellation of a parent group propagates to descendant groups;
- group exit waits for canceled children to run their cleanup paths; and
- an unobserved child failure is reported to its owning group.

`run-async` returns the root task's values after the root group has reached a
terminal state. If the root task fails, `run-async` raises the failure in its
caller. If the root task is canceled, `run-async` raises the cancellation
condition.

Tasks remain part of a task group. Long-lived services use an explicit task
group whose lifetime is owned by the application.

## Suspension protocol

The scheduler runs each task under its private prompt. The conceptual shape
is:

```scheme
(reset0-at scheduler-tag
  (run-task-entry task))
```

An operation that cannot complete immediately suspends through the matching
prompt:

```scheme
(shift0-at scheduler-tag k
  (register-wait! operation scheduler task
                  (make-one-shot-resumption task k)))
```

The actual implementation uses internal procedures so that the prompt tag,
task state transition, and resumption construction form one checked
operation.

Suspension performs these actions atomically from the scheduler's point of
view:

1. Capture the continuation through the scheduler prompt.
2. Wrap it in a one-shot resumption.
3. Change the task state from `running` to `waiting`.
4. Publish the wait registration.
5. Return control to the scheduler.

A successful completion claims the resumption, stores the operation result,
changes the task to `ready`, and schedules it for the next turn. Cancellation
and failure compete for the same claim. A second claim has no effect and
cannot invoke the continuation again.

Although Chez Scheme delimited continuations are multi-shot, async task
resumptions are one-shot. Multi-shot resumption would duplicate task identity,
resource ownership, and pending I/O state.

## Scheduler turns and fairness

Scheduling uses separate current-turn and next-turn queues. A task scheduled
while a turn is running enters the next-turn queue, including a task that
explicitly yields. A task therefore runs at most once per turn.

Each turn performs:

1. Drain remote submissions and completed libuv requests.
2. Move the next-turn queue into the current-turn queue when necessary.
3. Run every current-turn task at most once.
4. Process cancellations and handle-close completions.
5. Poll libuv.
6. Promote newly ready tasks for the next turn.

When runnable work exists, the scheduler calls `uv_run` in nonblocking mode.
When no task is runnable, it calls `uv_run` in a mode that waits for one event,
a timer, or a remote wakeup. Callback processing is bounded per turn so that
a sustained stream of I/O completions cannot starve runnable Scheme tasks.

`task-yield` schedules the current task for the next turn and returns true
after resumption. Outside an active async scheduler it returns false.

## Operations

An operation has four logical components:

```text
try       Attempt immediate completion without capturing a continuation.
block     Publish a waiter after the task has suspended.
wrap      Transform successful result values.
nack      Withdraw a waiter that lost a choice or was canceled.
```

`perform-operation` first calls `try`. A successful try returns directly. A
failed try suspends the task and calls `block` with the task resumption.

Operations use an atomic synchronization state with the logical states:

```text
waiting -> claimed -> synchronized
   |
   +---------------------> canceled
```

The claimed state serializes two participants that are attempting to commit a
channel exchange or a choice. Implementations may temporarily release a claim
back to `waiting` when the peer has already synchronized elsewhere.

`choice-operation` attempts its children in a rotating or randomized order.
If no child completes immediately, it registers all children against one
shared synchronization state. The winning child synchronizes the state and
nacks every losing child before resuming the task.

Performing an operation outside `run-async` is an async-context violation. The
library does not create hidden permanent polling threads.

## Channels and futures

Channels are thread-safe synchronization points. An unbuffered channel pairs
one sender with one receiver. A bounded channel permits up to its configured
capacity and applies backpressure when full.

Channel closure is a linearized state transition under the channel mutex.
The first close retains its reason, rejects blocked and subsequent sends, and
wakes blocked receivers. Buffered values drain before receivers observe the
closed state. The two-value receive form returns `(values value #t)` for a
value and `(values #f #f)` after closure; the single-value get form raises a
channel-closed condition after the buffer drains.

Channel send and receive are operations, so they compose with timers, task
joins, and I/O:

```scheme
(perform-operation
  (choice-operation
    (channel-get-operation input)
    (wrap-operation (sleep-operation 5)
      (lambda () (raise (make-timeout-condition))))))
```

A future is a single-assignment cell backed by a completion operation. It can
be fulfilled exactly once and can have multiple waiters. Task join may share
the same internal waiter machinery.

Queue cleanup is incremental. Completed or canceled waiter records are
removed during ordinary queue operations, with occasional bounded pruning to
avoid retaining dead resumptions.

## Fiber-aware mutexes

The `(chezscheme async sync)` library provides mutexes whose contended
acquisition suspends the current task. Mutex ownership is attached to the task
rather than its scheduler thread, so a task retains ownership while it yields,
waits for an operation, is preempted, or migrates within its scheduler group.

Waiters receive ownership in FIFO registration order. Ownership is published
before a selected waiter can resume on another scheduler. Cancellation or a
losing choice removes its waiter through the operation nack protocol. Mutexes
are nonrecursive, and release requires the current task to be the owner.

The scoped procedure and `with-async-mutex` syntax release ownership after a
normal return or an exception, including task cancellation. They preserve all
values returned by the protected body. Task termination releases any remaining
unscoped acquisitions before publishing the task's terminal state.

## Syntax layer

The `(chezscheme async syntax)` library is a hygienic, expression-oriented
layer over tasks, operations, contexts, channels, and synchronization. It does
not introduce a second scheduler abstraction. `async` delimits scheduler
execution, `go`
spawns a migratable structured child, and `await` joins a task while preserving
all result values.

`select-operation` constructs an ordinary operation from branch clauses;
`select` performs the constructed operation. A general `on` clause accepts any
operation. The `recv`, `send`, and `after` clauses expand to channel and timer
operations, while `else` supplies an immediately ready arm. Clause input
expressions are evaluated once in source order, only the committed body runs,
and losing arms receive ordinary operation nacks. These rules allow selection
to nest inside wrapping, choice, context, and I/O compositions without special
scheduler support.

Dynamic scope forms install timeout and explicit cancellation contexts.
`with-cancel-scope` binds an explicit cancellation procedure and cancels its
child context on scope exit, bounding the tasks and operations created in that
extent. `channel-for` implements the two-value channel receive protocol as an
iteration: it drains buffered values, stops after closed-and-empty, and leaves
channel ownership with the producer. `with-async-mutex` evaluates a body while
the current task owns a fiber-aware mutex.

## Cancellation

Cancellation is cooperative, thread-safe, and idempotent. Contexts form a
parent-to-child propagation tree. Canceling a parent recursively cancels its
children, while canceling a child leaves its parent and siblings active. The
first cancellation reason wins. A deadline cancels its context with the
`deadline-exceeded` reason.

Each context exposes a reusable done operation. `perform-operation` races the
current context's done operation against the requested operation. The shared
synchronization claim selects exactly one result; context cancellation nacks
the losing operation before resuming the task with an async cancellation
condition.

`task-cancel!` cancels the task-owned context and performs one of the
following actions:

- a ready task remains ready and observes cancellation before running user
  code;
- a running task observes cancellation at its next cancellation point; or
- a waiting task nacks its operation and is resumed with a cancellation
  condition.

Suspension, task yield, join, channel operations, timers, and async I/O are
cancellation points. CPU-bound code observes cancellation when it next enters
a cancellation point.

Cancellation does not discard a suspended continuation. The task is resumed
with a cancellation condition so that Scheme cleanup code runs. A task reaches
the `canceled` terminal state only after this unwind completes.

`uv_cancel` is used only for libuv request types that support it. Logical
cancellation remains authoritative: a callback arriving after cancellation
may release native resources, but it cannot claim or resume the task again.

## Exceptions

Every transition from the scheduler into user Scheme code has an exception
boundary. An uncaught condition changes the running task to `failed`, stores
the condition, wakes joiners, and notifies the parent task group. A condition
never escapes through a libuv C callback.

Operation completion represents either result values or a condition. When a
failed operation resumes its task, it raises the condition at the suspension
point. I/O conditions retain the libuv error code and include the operation,
handle, and relevant address or path information.

Each spawned task inherits an independent exception state from its parent.
Internal scheduler prompts are not part of a saved exception state.

## Dynamic state

Fiber-local dynamic state is required even when multiple fibers share one
operating-system thread. A spawned task inherits the values of its parent's
parameters, while subsequent parameter mutations are isolated between tasks.

The runtime captures and activates a dynamic-state snapshot that includes
ordinary dynamic parameter state and thread-parameter values. Snapshot
activation preserves the normal interaction between `parameterize`,
continuation invocation, exception state, and continuation marks.

Scheduler ownership, the currently running task, and libuv callback state are
kept in scheduler-controlled storage rather than inherited task parameters.
Activating a task cannot replace these scheduler invariants.

Each task carries a versioned dynamic-state snapshot that is activated before
the task enters user code. Parameter changes, exception state, and
continuation marks therefore remain fiber-local when a ready task migrates to
another scheduler thread. Scheduler ownership and native resource ownership
are reinstalled from scheduler-controlled state rather than inherited from
the task snapshot.

## Dynamic winders

Delimited continuation suspension leaves the dynamic extent of the task, so
ordinary `dynamic-wind` guards run during suspension and resumption. This is
the correct Scheme behavior but is unsuitable for cleanup constructs that
must remain active while a task is merely parked.

The async library provides an async-aware dynamic-wind operation. Its guards
are skipped while a task is being suspended, resumed, yielded, or preempted.
They run for normal entry and exit, exceptions, and cancellation.

The scheduler records whether a continuation transfer is a scheduling switch.
This state is true while unwinding a suspended task and while rewinding it for
resumption, then becomes false before user code continues. Cancellation first
resumes the task as a scheduling switch and then raises the cancellation
condition with scheduling-switch state disabled, ensuring cleanup runs once.

Async resource-management combinators use the async-aware operation. Ordinary
`dynamic-wind` retains its existing semantics.

## libuv integration

The C shim owns the concrete layouts of `uv_loop_t`, `uv_handle_t`, and
`uv_req_t`. Scheme code refers to native objects through sealed records and
stable identifiers.

The integration observes these rules:

- Native handles and requests are allocated outside the Scheme heap.
- A Scheme registry keeps requests, handles, buffers, and resumptions alive
  until native completion or close completion.
- A libuv `data` field contains a stable identifier or C-owned context, not an
  unprotected Scheme object pointer.
- Every `uv_close` target remains alive until its close callback runs.
- A request completion can enqueue a Scheme completion only on its owning
  scheduler.
- A libuv worker callback executes no Scheme code. Its completion callback
  transfers the result to the scheduler thread.
- A Scheme exception is caught before control returns through a C callback.

An async stream owns its descriptor and buffering policy. An ordinary Chez
Scheme buffered port and a libuv stream never operate concurrently on the same
descriptor. Port adapters transfer ownership explicitly and define what
happens to buffered input and output.

Write operations retain or copy their source bytevectors until completion.
Read operations retain their destination bytevectors and bounds until
completion. The C shim validates all ranges before submitting a request.

The I/O surface includes:

- monotonic timers;
- TCP and local-domain stream listen, accept, connect, read, write, shutdown,
  and close;
- forward and reverse name resolution;
- UDP sockets, datagram transfer, addressing, multicast membership, and
  socket options;
- asynchronous filesystem requests, streaming directory handles, and file
  port adapters;
- descriptor polling, processes with stdio pipes, and signal watchers;
- filesystem event and polling watchers;
- TTY streams and terminal control; and
- random data, system snapshots, and loop metrics.

## Scheduler groups and parallelism

The default scheduler group has one scheduler. An explicit parallelism option
creates one scheduler and one libuv loop per operating-system thread.

Tasks are scheduler-local unless created as migratable. A task is pinned while
it runs under an engine resumption or another scheduler-local execution
constraint. Native resources remain assigned to their owner loop independently
of the task that uses them. An operation on a resource from another scheduler
in the same group is submitted to the owner loop and its result is routed back
through the task's current scheduler.

Idle schedulers may steal only ready, migratable tasks. Waiting tasks are not
stolen; their completion is delivered to the scheduler that owns the wait.
After completion, an unpinned task is eligible for migration again. A waiting
task remains registered with the scheduler that owns its current suspension;
only ready tasks enter the stealable work deque.

Cross-scheduler channels and task joins use atomic queues. Publishing remote
work wakes the destination loop with `uv_async_send`. Scheduler-local I/O
registries require no cross-thread mutation.

Parallel execution requires complete fiber-local dynamic-state support.
Without that support, the scheduler rejects parallelism greater than one.

## Timed preemption

The cooperative scheduler is the core execution contract. Timed preemption is
an optional policy implemented with Chez Scheme engines.

In preemptive mode, the scheduler runs a ready task with a bounded number of
engine ticks. Normal completion produces task results. Engine expiration
produces a resumable task and schedules it for the next turn. Explicit async
suspension registers the resumable task with its operation instead of placing
it immediately on a run queue.

Preemption is disabled while executing scheduler internals, libuv callbacks,
foreign critical sections, and non-fiber-aware lock operations. Engines cannot
be nested; entering a preemptive scheduler while another engine is active is
an error.

Preemption is enabled per `run-async` invocation with a positive
`preemption-ticks` value. The default scheduler is cooperative. Engine
resumptions add a temporary affinity reason so that a preempted continuation
resumes on the scheduler that owns its engine state.

## Blocking operations

A scheduler thread must not perform an operation that can block outside
libuv. Regular-file access, name resolution, subprocess waits, and third-party
libraries may block even when socket operations do not.

Blocking C work runs in the libuv worker pool or an explicitly managed worker
pool. Worker functions receive C-owned input and produce C-owned results. The
completion path converts the result to Scheme on the scheduler thread.

Scheme procedures do not run in a libuv worker. CPU-bound Scheme work uses a
separate Chez Scheme thread or a parallel scheduler task.

Fiber-aware synchronization uses channels, operations, futures, or async
mutexes. Holding an operating-system mutex across a suspension is invalid
because another fiber on the same thread cannot make progress through that
mutex. Internal operating-system mutex sections disable timer interrupts so
engine expiration cannot suspend a task while it owns a scheduler lock.

## Resource finalization

Explicit close and structured task cleanup are the primary resource lifetime
mechanisms. Guardians provide a fallback for unreachable handles, but a
guardian never directly closes a live libuv handle from an arbitrary thread.
It submits a close command to the owning scheduler.

Closing a stream wakes all pending readers and writers with a closed-handle
condition. Descriptor reuse cannot connect an old waiter to a new handle,
because waiters are associated with handle identity and generation rather
than a raw descriptor alone.

Destroying a scheduler performs an orderly shutdown:

1. Reject new tasks and operations.
2. Request cancellation of owned task groups.
3. Resume waiting tasks so their cleanup paths run.
4. Close all libuv handles.
5. Run the loop until close callbacks complete.
6. Release the loop and native registries.

## Observability

Tasks expose stable identifiers, optional names, state, owning scheduler, and
a description of the current wait. Scheduler inspection exposes task, turn,
suspension, and wakeup counters. The system I/O library exposes libuv loop
time, idle time, backend state, loop counts, and event counts.

Wait descriptions identify task join, channel send or receive, timer deadline,
and I/O operation without retaining additional user objects solely for
diagnostics. Backtrace support may inspect a suspended continuation when the
runtime can do so safely.

## Testing strategy

The scheduler backend has a deterministic test implementation with a virtual
monotonic clock and explicitly delivered completion events. Most task,
operation, channel, cancellation, and fairness tests run without real time or
network access.

Required test classes include:

- suspension and resumption with zero, one, and multiple values;
- enforcement of one-shot resumptions;
- FIFO and turn fairness;
- immediate operation completion without continuation capture;
- completion, timeout, and cancellation races;
- choice operations with exactly one winner;
- channel close races with buffered values, blocked sends, and blocked
  receives;
- multiple joiners and exception propagation;
- parent, descendant, deadline, and cross-thread context cancellation;
- cleanup through async-aware dynamic winders;
- parameter and exception-state isolation;
- handle close while reads and writes are pending;
- descriptor reuse after close;
- remote submission and loop wakeup;
- scheduler shutdown with live tasks and handles;
- task migration and affinity when parallelism is enabled; and
- preemption around Scheme, exception, and foreign-call boundaries.

Stress tests repeatedly race completion against cancellation and run channel
operations across scheduler threads. Native integration tests run under memory
and thread sanitizers where supported.

## Capability layers

The implementation is organized into these capability layers:

1. A single-thread cooperative scheduler with private prompts, task records,
   one-shot resumptions, joining, cancellation, and deterministic tests.
2. Fiber-local dynamic-state capture and activation.
3. The operation protocol, timers, futures, closeable channels, choice, and
   hierarchical cancellation contexts.
4. The libuv shim, async streams, name resolution, filesystem requests, and
   orderly native shutdown.
5. Scheduler groups, remote wakeup, task affinity, and ready-task work
   stealing.
6. Opt-in timed preemption using engines.
7. Explicit adapters between async handles and Chez Scheme ports.

Each layer preserves the task and operation contracts of the layers beneath
it. Parallel execution requires thread support and a real clock. Timed
preemption requires an available, non-nested engine context.
