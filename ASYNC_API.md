# Asynchronous Fibers API Reference

## Overview

The asynchronous API provides fiber scheduling, cancellation contexts,
composable wait operations, channels, and libuv-backed I/O. The scheduler and
operation contracts are described in [ASYNC.md](ASYNC.md). This document
specifies the public Scheme libraries, procedure signatures, options, and
result formats.

Procedures described as blocking suspend only the current async task. They
must run inside the dynamic extent of `run-async`. Procedures whose names end
in `-operation` construct an operation without performing it; the operation is
executed by `perform-operation` and can be combined with `choice-operation` or
`wrap-operation`.

Native handles belong to the scheduler group in which they are created. A
task in another scheduler of the same group may use the handle; the request is
routed to the handle's owner loop. Handles cannot be used from a different
scheduler group.

## `(chezscheme async)`

### Running schedulers and tasks

```scheme
(run-async thunk option value ...) -> values ...
```

Runs `thunk` as the root task and returns its values after its structured task
group terminates. Supported options are:

- `clock`: `real` or `virtual`; the default is `real`.
- `parallelism`: a positive fixnum; the default is `1`. Values greater than
  one require thread support and a real clock.
- `preemption-ticks`: `#f` or a positive fixnum; the default is `#f`.

A scheduler using `preemption-ticks` cannot be nested inside an active engine.

```scheme
(spawn-task thunk option value ...) -> task
```

Creates a child task. Supported options are:

- `name`: an application-defined task name.
- `group`: an explicit task group.
- `context`: an explicit parent cancellation context. The task owns a new
  child of this context.
- `migratable?`: a boolean controlling eligibility for work stealing; the
  default is `#f`.

```scheme
(task? object) -> boolean
(task-id task) -> exact-integer
(task-name task) -> object-or-#f
(task-state task) -> symbol
(task-scheduler task) -> scheduler
(task-current-wait task) -> object-or-#f
(task-context task) -> async-context
(current-async-task) -> task-or-#f
(current-async-scheduler) -> scheduler-or-#f
```

Task states are `created`, `ready`, `running`, `waiting`, `completed`,
`failed`, and `canceled`. A current-wait value is diagnostic data describing
the operation on which a task is suspended.

```scheme
(task-join task) -> values ...
(task-join-operation task) -> operation
(task-cancel! task [reason]) -> void
(task-yield) -> boolean
(async-sleep seconds) -> void
```

`task-join` returns the task's values or raises its stored failure or
cancellation condition. A task cannot join itself. `task-cancel!` is
idempotent and cancels the task's context. The first cancellation reason is
retained. `task-yield` returns `#t` after yielding an async task and `#f`
outside an async task. `seconds` is a nonnegative real number.

`task-join-operation` constructs the operation performed by `task-join`.
When it loses a choice, its waiter is withdrawn without canceling the target
task. Performing an operation that targets the current task is an error.

### Task groups and cancellation

```scheme
(make-task-group) -> task-group
(task-group? object) -> boolean
(task-group-wait task-group) -> void
```

`task-group-wait` waits for all children and raises an unobserved child
failure. A task spawned with an explicit `group` belongs to that group and,
unless it has an explicit `context` option, receives a child of the context
captured when the group was created.

```scheme
(make-async-cancellation-condition [reason]) -> condition
(async-cancellation-condition? object) -> boolean
(async-cancellation-reason condition) -> object
```

### Dynamic extent and scheduler counters

```scheme
(async-dynamic-wind before thunk after) -> values ...
```

This form has the ordinary `dynamic-wind` interface. Its guards are skipped
for scheduler suspension, resumption, yield, and preemption transfers. They
run for normal entry and exit, exception unwinding, and cancellation cleanup.

```scheme
(async-scheduler? object) -> boolean
(async-scheduler-task-count scheduler) -> exact-integer
(async-scheduler-turn-count scheduler) -> exact-integer
(async-scheduler-suspension-count scheduler) -> exact-integer
(async-scheduler-wakeup-count scheduler) -> exact-integer
```

The task count covers the scheduler group. The remaining counters apply to
the supplied scheduler.

### Examples

Join a child task and preserve all of its return values:

```scheme
(import (chezscheme) (chezscheme async))

(run-async
  (lambda ()
    (let ([worker
           (spawn-task
             (lambda () (values 'answer 42))
             'name 'worker
             'migratable? #t)])
      (let-values ([(label value) (task-join worker)])
        (list label value))))
  'parallelism 2)
;; => (answer 42)
```

Use async-aware cleanup around a suspension:

```scheme
(import (chezscheme) (chezscheme async))

(run-async
  (lambda ()
    (async-dynamic-wind
      (lambda () (display "enter\n"))
      (lambda () (async-sleep 0.01) 'done)
      (lambda () (display "leave\n")))))
;; prints enter and leave once; returns done
```

## `(chezscheme async context)`

### Context creation and cancellation

```scheme
(make-async-context [parent]) -> async-context
(async-context? object) -> boolean
(async-context-cancel! context [reason]) -> void
(async-context-canceled? context) -> boolean
(async-context-reason context) -> object
```

Without an explicit parent, `make-async-context` uses the current async
context when one exists. Passing `#f` creates an independent root. Parent
cancellation propagates to descendants; child cancellation does not affect
its parent or siblings. Cancellation is thread-safe and idempotent, and the
first reason is retained.

Every task owns a context. A spawned task receives a child of its explicit
`context` option, otherwise of its explicit task group's context, otherwise
of the current context. A terminal task cancels its owned context so
descendant scopes cannot outlive it.

### Operations and dynamic scopes

```scheme
(async-context-done-operation context) -> operation
(perform-operation/context context operation) -> values ...
(current-async-context) -> async-context-or-#f
(call-with-async-context context thunk) -> values ...
```

The done operation completes without values when its context is canceled and
can participate in `choice-operation`. `perform-operation/context` performs
an operation while racing it against context cancellation; cancellation
nacks the operation and raises an async cancellation condition carrying the
context reason.

`call-with-async-context` installs a fiber-local current context for `thunk`.
Ordinary `spawn-task` calls in that extent inherit it. All operations
performed by a task are also bounded by its current context.

```scheme
(async-context-with-timeout parent seconds) -> async-context
(async-context-with-deadline parent monotonic-time) -> async-context
(call-with-async-timeout seconds thunk) -> values ...
```

Timeout and deadline creation require an active async task. Relative timeouts
use the scheduler clock and therefore work with real and virtual schedulers.
Absolute deadlines accept a `time-monotonic` time object and require a real
scheduler clock. Expiry uses the reason `deadline-exceeded`.
`call-with-async-timeout` installs a child context for the dynamic extent of
`thunk` and releases its deadline timer on exit.

### Examples

Bound a group of operations with one timeout:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async context))

(run-async
  (lambda ()
    (guard (c [(async-cancellation-condition? c)
               (async-cancellation-reason c)])
      (call-with-async-timeout 0.1
        (lambda ()
          (async-sleep 10)
          'completed)))))
;; => deadline-exceeded
```

Cancel descendants from another task:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async context))

(run-async
  (lambda ()
    (let* ([context (make-async-context)]
           [worker
            (spawn-task
              (lambda () (async-sleep 10))
              'context context)])
      (async-context-cancel! context 'shutdown)
      (guard (c [(async-cancellation-condition? c)
                 (async-cancellation-reason c)])
        (task-join worker)))))
;; => shutdown
```

## `(chezscheme async operations)`

```scheme
(operation? object) -> boolean
(perform-operation operation) -> values ...
(make-operation try block [wrap [nack]]) -> operation
```

`make-operation` is the low-level operation constructor. `try` receives a
synchronization state and returns `#f` when the operation must block, a
`(values . values)` payload on success, or a `(raise . condition)` payload on
failure. `block` receives the synchronization state and a one-shot delivery
procedure. `wrap` transforms a list of successful values. `nack` withdraws a
registered waiter.

```scheme
(always-operation value ...) -> operation
(never-operation) -> operation
(sleep-operation seconds) -> operation
(wrap-operation operation procedure) -> operation
(choice-operation operation ...) -> operation
```

`choice-operation` commits exactly one child. Its starting child rotates
between attempts. A choice with no children behaves like `never-operation`.

```scheme
(make-future) -> future
(future? object) -> boolean
(future-fulfil! future value ...) -> void
(future-fail! future condition) -> void
(future-operation future) -> operation
(future-get future) -> values ...
```

A future is single-assignment. A second fulfilment or failure is an error.

### Examples

Race a channel receive against a timeout:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async operations)
        (chezscheme async channels))

(run-async
  (lambda ()
    (let ([input (make-channel)])
      (spawn-task
        (lambda ()
          (async-sleep 0.01)
          (channel-put input "ready")))
      (perform-operation
        (choice-operation
          (channel-get-operation input)
          (wrap-operation (sleep-operation 1)
            (lambda () "timed out")))))))
;; => "ready"
```

Fulfil a future from another task:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async operations))

(run-async
  (lambda ()
    (let ([result (make-future)])
      (spawn-task (lambda () (future-fulfil! result 1 2 3)))
      (call-with-values
        (lambda () (future-get result))
        list))))
;; => (1 2 3)
```

## `(chezscheme async channels)`

```scheme
(make-channel [capacity]) -> channel
(channel? object) -> boolean
(channel-close! channel [reason]) -> void
(channel-closed? channel) -> boolean
(channel-put-operation channel value) -> operation
(channel-get-operation channel) -> operation
(channel-receive-operation channel) -> operation
(channel-put channel value) -> void
(channel-get channel) -> object
(channel-receive channel) -> value open?
```

The default capacity is zero, producing an unbuffered rendezvous channel. A
positive capacity creates a bounded FIFO buffer. Channel operations are safe
between schedulers and operating-system threads in the same process.

Closing a channel is idempotent, and the first close reason is retained.
Buffered values remain available in FIFO order. After the buffer is empty,
`channel-receive` returns `(values #f #f)`. A successful receive returns the
value and `#t`; this distinguishes a transmitted `#f` from a closed channel.

Blocked and subsequent puts fail when the channel closes. `channel-get`
raises the same condition after the buffer is empty, while
`channel-receive` provides the two-value consumption protocol:

```scheme
(make-channel-closed-condition [reason]) -> condition
(channel-closed-condition? object) -> boolean
(channel-closed-reason condition) -> object
```

### Examples

Use a bounded channel for producer backpressure:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async channels))

(run-async
  (lambda ()
    (let ([queue (make-channel 2)])
      (let ([producer
             (spawn-task
               (lambda ()
                 (for-each (lambda (v) (channel-put queue v)) '(a b c))))])
        (let ([values
               (list (channel-get queue)
                     (channel-get queue)
                     (channel-get queue))])
          (task-join producer)
          values)))))
;; => (a b c)
```

Close a producer-owned channel and consume its remaining values:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async channels))

(run-async
  (lambda ()
    (let ([values (make-channel 2)])
      (spawn-task
        (lambda ()
          (channel-put values 1)
          (channel-put values 2)
          (channel-close! values)))
      (let loop ([items '()])
        (let-values ([(value open?) (channel-receive values)])
          (if open?
              (loop (cons value items))
              (reverse items)))))))
;; => (1 2)
```

## `(chezscheme async syntax)`

This library provides hygienic expression-oriented forms over the task,
context, operation, and channel APIs. It introduces no separate scheduler or
runtime state. Forms preserve all values produced by their bodies and do not
intercept exceptions or cancellation conditions.

```scheme
(async body ...) -> values ...
(async/options ([option expression] ...) body ...) -> values ...
```

`async` invokes `run-async` with the body in a thunk. `async/options` accepts
the `clock`, `parallelism`, and `preemption-ticks` options of `run-async`.
Option expressions are evaluated exactly once in source order before the
scheduler starts.

```scheme
(go body ...) -> task
(go/options ([option expression] ...) body ...) -> task
(await task-expression) -> values ...
```

`go` spawns a migratable child task. `go/options` accepts the `name`, `group`,
`context`, and `migratable?` options of `spawn-task`; it also defaults
`migratable?` to `#t`. Option expressions are evaluated exactly once in source
order before the task is spawned. `await` joins the task produced by
`task-expression`.

```scheme
(select-operation clause ...) -> operation
(select clause ...) -> values ...

clause = [(on operation-expression variable ...) body ...]
       | [(recv channel-expression value-variable open?-variable) body ...]
       | [(send channel-expression value-expression) body ...]
       | [(after seconds-expression) body ...]
       | [else body ...]
```

`select-operation` constructs a choice operation and `select` immediately
performs that operation. `on` accepts any operation and binds all values it
produces. `recv`, `send`, and `after` are shorthands for channel receive,
channel put, and sleep operations. An `else` clause must be last and is ready
immediately. Without `else`, the selection suspends until an arm is ready.

All arm input expressions are evaluated exactly once in clause order before
the choice is constructed. Only the selected body runs. Losing operations are
nacked according to the ordinary `choice-operation` contract. The result of
the selected body becomes the result of the operation, so a
`select-operation` can itself be wrapped, combined, or performed under a
cancellation context.

```scheme
(with-timeout seconds-expression body ...) -> values ...
(with-async-context context-expression body ...) -> values ...
(with-cancel-scope (cancel!) body ...) -> values ...
```

`with-timeout` installs a timeout context for the body.
`with-async-context` installs the context produced by `context-expression`.
`with-cancel-scope` installs a fresh child context and lexically binds
`cancel!` as a procedure accepting zero or one cancellation-reason argument.
The scope context is canceled with `scope-exited` when control leaves the
body, which also bounds descendants created in the scope.

```scheme
(channel-for (value-variable channel-expression) body ...) -> void
```

`channel-for` evaluates `channel-expression` once and receives values until
the channel is closed and empty. Buffered values are drained before the loop
terminates. The form neither closes the channel nor cancels its producer.

Spawn two producers, select their next available value, and consume a closed
channel:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async channels)
        (chezscheme async syntax))

(async
  (let ([left (make-channel 1)]
        [right (make-channel 1)])
    (go (channel-put left 'left) (channel-close! left))
    (go (channel-put right 'right) (channel-close! right))
    (let ([first
           (select
             [(recv left value open?) (and open? value)]
             [(recv right value open?) (and open? value)])]
          [remaining '()])
      (channel-for (value right)
        (set! remaining (cons value remaining)))
      (cons first (reverse remaining)))))
```

## I/O conventions

Applications import each I/O domain library directly. A direct blocking
procedure and its `-operation` counterpart accept the same arguments and
produce the same successful values unless stated otherwise.

Explicit close is the primary resource-lifetime mechanism. Scheduler shutdown
and guardians close remaining native resources. Closing a port returned by an
async port adapter closes its underlying handle.

## `(chezscheme async io errors)`

```scheme
(make-async-io-condition operation handle path code) -> condition
(async-io-condition? object) -> boolean
(async-io-condition-operation condition) -> symbol-or-object
(async-io-condition-handle condition) -> object-or-#f
(async-io-condition-path condition) -> string-or-#f
(async-io-condition-code condition) -> integer-or-symbol
(async-io-error-name code) -> string
(async-io-error-message code) -> string
```

Native failures retain the libuv error code. Logical resource errors use a
symbol such as `closed` or `busy`.

### Examples

Inspect an I/O failure without discarding its native error code:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async io errors)
        (chezscheme async io fs))

(run-async
  (lambda ()
    (guard (c [(async-io-condition? c)
               (list (async-io-condition-operation c)
                     (async-io-condition-path c)
                     (async-io-error-name
                       (async-io-condition-code c)))])
      (file-open "/path/that/does/not/exist" '(read)))))
;; A typical result:
;; (open "/path/that/does/not/exist" "ENOENT")
```

## `(chezscheme async io stream)`

### TCP and local-domain listeners

```scheme
(tcp-listen host port [backlog]) -> listener
(pipe-listen path [backlog]) -> listener
(tcp-listener? object) -> boolean
(tcp-listener-close listener) -> void
(tcp-accept-operation listener) -> operation
(tcp-accept listener) -> stream
(tcp-connect-operation host port) -> operation
(tcp-connect host port) -> tcp-stream
(pipe-connect-operation path) -> operation
(pipe-connect path) -> pipe-stream
```

The default backlog is `128`. `tcp-listener?` recognizes both TCP and
local-domain listeners. `host` may be an IPv4 or IPv6 address accepted by
libuv, and `port` is in the range 0 through 65535.

### Streams and port adapters

```scheme
(async-stream? object) -> boolean
(tcp-stream? object) -> boolean
(pipe-stream? object) -> boolean
(stream-closed? object) -> boolean
(stream-read-operation stream) -> operation
(stream-read stream) -> bytevector-or-eof
(stream-write-operation stream bytevector) -> operation
(stream-write stream bytevector) -> void
(stream-shutdown stream) -> void
(stream-close stream) -> void
```

The write operation returns the byte count; the direct `stream-write`
procedure returns void. `stream-shutdown` shuts down the writable side after
pending writes complete. Reads may return any positive-size chunk and return
the end-of-file object after EOF.

```scheme
(async-stream->binary-input-port stream) -> binary-input-port
(async-stream->binary-output-port stream) -> binary-output-port
(async-stream->binary-input/output-port stream) -> binary-input/output-port
```

An adapter transfers ownership of the stream to the port. Stream procedures
reject subsequent direct access. Stream ports are not seekable.

### Examples

Run a TCP request and response on two fibers:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async io stream))

(run-async
  (lambda ()
    ;; Select an application-owned port appropriate for the environment.
    (let* ([listener (tcp-listen "127.0.0.1" 9000)]
           [server
            (spawn-task
              (lambda ()
                (let ([stream (tcp-accept listener)])
                  (let ([request (utf8->string (stream-read stream))])
                    (stream-write stream (string->utf8 "pong"))
                    (stream-close stream)
                    request))))]
           [client (tcp-connect "127.0.0.1" 9000)])
      (stream-write client (string->utf8 "ping"))
      (let ([reply (utf8->string (stream-read client))])
        (stream-close client)
        (tcp-listener-close listener)
        (list (task-join server) reply)))))
;; => ("ping" "pong")
```

## `(chezscheme async io dns)`

```scheme
(dns-lookup-operation node [service]) -> operation
(dns-lookup node [service]) -> list
```

The result is a list of `(host port family)` lists. `host` is numeric, `port`
is an integer, and `family` is `4` for IPv4 or `6` for IPv6. `service` may be
a string or `#f`.

```scheme
(dns-reverse-operation host port [flags]) -> operation
(dns-reverse host port [flags]) -> host-name service-name
```

The result consists of two values. Flags are `name-required`, `numeric-host`,
and `numeric-service`.

### Examples

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async io dns))

(run-async (lambda () (dns-lookup "localhost" "80")))
;; One possible result:
;; (("127.0.0.1" 80 4) ...)

(run-async
  (lambda ()
    (let-values ([(host service)
                  (dns-reverse "127.0.0.1" 80
                    '(numeric-host numeric-service))])
      (list host service))))
;; => ("127.0.0.1" "80")
```

## `(chezscheme async io udp)`

```scheme
(udp-open) -> udp-socket
(udp-open host port [bind-flags]) -> udp-socket
(udp-socket? object) -> boolean
(udp-close socket) -> void
(udp-bind! socket host port [bind-flags]) -> void
```

Bind flags are `ipv6-only` and `reuse-address`.

```scheme
(udp-connect! socket host port) -> void
(udp-disconnect! socket) -> void
(udp-send-operation socket bytevector) -> operation
(udp-send-operation socket bytevector host port) -> operation
(udp-send socket bytevector) -> exact-integer
(udp-send socket bytevector host port) -> exact-integer
(udp-receive-operation socket) -> operation
(udp-receive socket) -> bytevector host port family
```

The two-argument send form requires a connected socket. Receive returns four
values. The family value is `4` for IPv4 or `6` for IPv6. A zero-length
datagram is represented by an empty bytevector.

```scheme
(udp-local-address socket) -> host port family
(udp-peer-address socket) -> host port family
(udp-membership-set! socket multicast interface source action) -> void
(udp-multicast-interface-set! socket interface) -> void
(udp-option-set! socket option value) -> void
```

`action` is `join` or `leave`. An empty `source` selects ordinary multicast
membership; a nonempty value selects source-specific membership. UDP options
are `multicast-loop`, `multicast-ttl`, `broadcast`, and `ttl`.

### Examples

Exchange a datagram over the loopback interface:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async io udp))

(run-async
  (lambda ()
    ;; Select an application-owned port appropriate for the environment.
    (let ([server (udp-open "127.0.0.1" 9001)]
          [client (udp-open)])
      (udp-send client (string->utf8 "ping") "127.0.0.1" 9001)
      (let-values ([(payload host port family) (udp-receive server)])
        (udp-close client)
        (udp-close server)
      (list (utf8->string payload) host family)))))
;; One possible result:
;; ("ping" "127.0.0.1" 4)
```

## `(chezscheme async io fs)`

### Files and ports

```scheme
(async-file? object) -> boolean
(file-open-operation path flags [mode]) -> operation
(file-open path flags [mode]) -> async-file
(file-read-operation file length) -> operation
(file-read file length) -> bytevector-or-eof
(file-write-operation file bytevector) -> operation
(file-write file bytevector) -> exact-integer
(file-close-operation file) -> operation
(file-close file) -> void
```

Open flags are `read`, `write`, `create`, `truncate`, `append`, and
`exclusive`. The default creation mode is `#o666`. Operations on a file are
serialized when they use or change its implicit offset. `file-send` uses an
explicit input offset and serializes access to the output file. `length` must
be positive.

```scheme
(async-file->binary-input-port file) -> binary-input-port
(async-file->binary-output-port file) -> binary-output-port
(async-file->binary-input/output-port file) -> binary-input/output-port
```

An adapter transfers ownership to the port. Non-append files expose port
position operations; append-mode files do not.

### Metadata and path operations

Each operation constructor below has a direct form with the same arguments
and base name without `-operation`.

```scheme
(file-stat-operation path-or-file) -> operation
(file-lstat-operation path) -> operation
(file-system-stat-operation path) -> operation
(file-delete-operation path) -> operation
(file-rename-operation old-path new-path) -> operation
(file-copy-operation source target [flags]) -> operation
(directory-create-operation path [mode]) -> operation
(directory-delete-operation path) -> operation
```

`file-stat` and `file-lstat` return an alist containing `dev`, `mode`,
`nlink`, `uid`, `gid`, `rdev`, `ino`, `size`, `blksize`, `blocks`, `atime`,
`mtime`, and `ctime`. Each time is a `(seconds . nanoseconds)` pair.

`file-system-stat` returns `type`, `block-size`, `blocks`, `blocks-free`,
`blocks-available`, `files`, `files-free`, and `fragment-size` entries.

Copy flags are `exclusive`, `clone`, and `clone-force`. The default directory
mode is `#o755`.

```scheme
(file-stat path-or-file) -> alist
(file-lstat path) -> alist
(file-system-stat path) -> alist
(file-delete path) -> void
(file-rename old-path new-path) -> void
(file-copy source target [flags]) -> void
(directory-create path [mode]) -> void
(directory-delete path) -> void
```

```scheme
(file-sync-operation file) -> operation
(file-data-sync-operation file) -> operation
(file-truncate-operation file length) -> operation
(file-send-operation output input offset length) -> operation
(file-access-operation path modes) -> operation
(file-mode-set-operation path-or-file mode) -> operation
(file-times-set-operation path-or-file atime mtime [follow?]) -> operation
(file-owner-set-operation path-or-file uid gid [follow?]) -> operation
```

The corresponding direct mutators are `file-sync`, `file-data-sync`,
`file-truncate`, `file-send`, `file-access?`, `file-mode-set!`,
`file-times-set!`, and `file-owner-set!`. `file-send` returns the number of
bytes copied; the other mutators return void except `file-access?`, which
returns a boolean. Access modes are `exists`, `read`, `write`, and `execute`.
Timestamps are real seconds. `follow?` defaults to `#t` for path targets.

```scheme
(file-sync file) -> void
(file-data-sync file) -> void
(file-truncate file length) -> void
(file-send output input offset length) -> exact-integer
(file-access? path modes) -> boolean
(file-mode-set! path-or-file mode) -> void
(file-times-set! path-or-file atime mtime [follow?]) -> void
(file-owner-set! path-or-file uid gid [follow?]) -> void
```

```scheme
(file-link-operation source target) -> operation
(file-link source target) -> void
(file-symbolic-link-operation source target [flags]) -> operation
(file-symbolic-link source target [flags]) -> void
(file-read-link-operation path) -> operation
(file-read-link path) -> string
(file-real-path-operation path) -> operation
(file-real-path path) -> string
```

Symbolic-link flags are `directory` and `junction` where supported by the
platform.

### Directories and temporary paths

```scheme
(directory-scan-operation path) -> operation
(directory-scan path) -> list
(async-directory? object) -> boolean
(directory-open-operation path) -> operation
(directory-open path) -> async-directory
(directory-read-operation directory [count]) -> operation
(directory-read directory [count]) -> list
(directory-close-operation directory) -> operation
(directory-close directory) -> void
```

Directory entries are `(name . type)` pairs. Types are `file`, `directory`,
`link`, `fifo`, `socket`, `character-device`, `block-device`, and `unknown`.
`directory-read` defaults to at most 64 entries and returns an empty list at
the end. Operations on one streaming directory are serialized.

```scheme
(temporary-directory-create-operation pattern) -> operation
(temporary-directory-create pattern) -> string
(temporary-file-open-operation pattern) -> operation
(temporary-file-open pattern) -> async-file path
```

Patterns follow libuv's template requirements and end in six `X` characters.
`temporary-file-open` returns two values.

### Examples

On a POSIX system, use a temporary file through a seekable binary port.
Closing the port closes the underlying async file:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async io fs))

(run-async
  (lambda ()
    (let-values ([(file path)
                  (temporary-file-open "/tmp/chez-example-XXXXXX")])
      (let ([port (async-file->binary-input/output-port file)])
        (put-bytevector port (string->utf8 "hello"))
        (flush-output-port port)
        (set-port-position! port 0)
        (let ([text (utf8->string (get-bytevector-n port 5))])
          (close-port port)
          (file-delete path)
          text)))))
;; => "hello"
```

Read a large directory in bounded batches:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async io fs))

(run-async
  (lambda ()
    (let ([directory (directory-open "/tmp")])
      (let loop ([entries '()])
        (let ([batch (directory-read directory 32)])
          (if (null? batch)
              (begin
                (directory-close directory)
                (reverse entries))
              (loop (append (reverse batch) entries))))))))
;; => (("entry" . file) ...)
```

## `(chezscheme async io poll)`

```scheme
(fd-poll-open fd) -> poll-handle
(fd-poll-handle? object) -> boolean
(fd-poll-operation poll-handle events) -> operation
(fd-poll poll-handle events) -> list
(fd-poll-close poll-handle) -> void
```

Events are `readable`, `writable`, `disconnect`, and `prioritized`. The result
is the subset reported by libuv. A poll handle permits one active waiter and
does not own or close the descriptor.

### Examples

Wrap polling in a helper while leaving descriptor ownership with the caller:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async io poll))

(define await-readable
  (lambda (fd)
    (run-async
      (lambda ()
        (let ([poll (fd-poll-open fd)])
          (let ([events (fd-poll poll '(readable disconnect))])
            (fd-poll-close poll)
            events))))))
```

## `(chezscheme async io process)`

```scheme
(process-spawn file arguments [options])
  -> process stdin-stream-or-#f stdout-stream-or-#f stderr-stream-or-#f
```

`arguments` is a list of strings following the executable name. `options` is
an alist with these keys:

- `cwd`: working-directory string.
- `environment`: an alist of string keys and string values. `#f` inherits the
  parent environment.
- `flags`: a list containing `detached`, `windows-hide`, or
  `windows-verbatim-arguments`.
- `stdin`, `stdout`, and `stderr`: `pipe`, `ignore`, `inherit`, or a
  nonnegative descriptor.

The stdio defaults are `pipe`. Returned pipes use the stream API.

```scheme
(async-process? object) -> boolean
(process-id process) -> exact-integer
(process-wait-operation process) -> operation
(process-wait process) -> exit-status termination-signal
(process-kill process signal) -> void
(process-id-kill pid signal) -> void
(process-close process) -> void
```

`process-close` releases the libuv process handle; it does not terminate the
child. Use `process-kill` when termination is required.

### Examples

On a POSIX system, capture a child process's standard output:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async io process)
        (chezscheme async io stream))

(run-async
  (lambda ()
    (let-values ([(child stdin stdout stderr)
                  (process-spawn "/bin/sh" '("-c" "printf hello"))])
      (stream-close stdin)
      (let ([text (utf8->string (stream-read stdout))])
        (let-values ([(status signal) (process-wait child)])
          (stream-close stdout)
          (stream-close stderr)
          (process-close child)
          (list text status signal))))))
;; => ("hello" 0 0)
```

## `(chezscheme async io signal)`

```scheme
(signal-open signum) -> signal-watcher
(signal-watcher? object) -> boolean
(signal-receive-operation watcher) -> operation
(signal-receive watcher) -> signum
(signal-close watcher) -> void
```

Each receive is a one-shot wait. Call `signal-receive` again to await another
occurrence.

### Examples

Wait for one occurrence and close the watcher even when the wait exits by an
exception or cancellation. Signal numbers and their meanings are
platform-specific:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async io signal))

(define receive-one-signal
  (lambda (signum)
    (run-async
      (lambda ()
        (let ([watcher (signal-open signum)])
          (async-dynamic-wind
            (lambda () (void))
            (lambda () (signal-receive watcher))
            (lambda () (signal-close watcher))))))))
```

## `(chezscheme async io watch)`

```scheme
(fs-event-open path [flags]) -> event-watcher
(fs-event-watcher? object) -> boolean
(fs-event-receive-operation watcher) -> operation
(fs-event-receive watcher) -> name events
(fs-event-close watcher) -> void
```

Flags are `watch-entry`, `stat`, and `recursive`, subject to platform support.
The result contains a filename string and a list containing `rename` and/or
`change`.

```scheme
(fs-poll-open path interval-ms) -> poll-watcher
(fs-poll-watcher? object) -> boolean
(fs-poll-receive-operation watcher) -> operation
(fs-poll-receive watcher) -> previous-stat current-stat
(fs-poll-close watcher) -> void
```

The two stat values use the file-stat alist format. Each receive waits for one
observed change.

### Examples

Receive one filesystem event and release the watcher afterward:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async io watch))

(define watch-one-change
  (lambda (path)
    (run-async
      (lambda ()
        (let ([watcher (fs-event-open path)])
          (async-dynamic-wind
            (lambda () (void))
            (lambda ()
              (let-values ([(name events)
                            (fs-event-receive watcher)])
                (list name events)))
            (lambda () (fs-event-close watcher))))))))

;; After another process changes path, one possible result is:
;; ("entry" (rename change))
```

Use polling when event watching is unavailable or stat snapshots are needed:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async io watch))

(define poll-one-change
  (lambda (path)
    (run-async
      (lambda ()
        (let ([watcher (fs-poll-open path 250)])
          (async-dynamic-wind
            (lambda () (void))
            (lambda ()
              (let-values ([(previous current)
                            (fs-poll-receive watcher)])
                (list previous current)))
            (lambda () (fs-poll-close watcher))))))))
```

## `(chezscheme async io tty)`

```scheme
(tty-open fd) -> tty-stream
(tty-stream? object) -> boolean
(tty-mode-set! tty mode) -> void
(tty-window-size tty) -> width height
(tty-virtual-terminal-state) -> supported-or-unsupported
(tty-virtual-terminal-state-set! state) -> void
(tty-reset-mode!) -> void
```

TTY modes are `normal`, `raw`, and `io`. Virtual-terminal states are the
symbols `supported` and `unsupported`. A TTY stream also satisfies
`async-stream?` and uses the stream read, write, close, and port-adapter API.

### Examples

Temporarily place a terminal in raw mode. The descriptor must refer to a TTY;
the caller supplies the interaction performed while raw mode is active:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async io stream)
        (chezscheme async io tty))

(define call-with-raw-tty
  (lambda (fd proc)
    (run-async
      (lambda ()
        (let ([tty (tty-open fd)])
          (async-dynamic-wind
            (lambda () (tty-mode-set! tty 'raw))
            (lambda () (proc tty))
            (lambda ()
              (tty-mode-set! tty 'normal)
              (stream-close tty))))))))

;; Read one chunk from standard input while it is in raw mode:
;; (call-with-raw-tty 0 stream-read)
```

## `(chezscheme async io system)`

```scheme
(random-bytevector-operation length) -> operation
(random-bytevector length) -> bytevector
(system-high-resolution-time) -> exact-integer
(system-memory-info) -> alist
(system-uptime) -> real
(system-load-average) -> list
(system-process-info) -> alist
(system-path-info) -> alist
(system-uname) -> alist
(system-cpu-info) -> list
(system-interface-info) -> list
(system-resource-usage) -> alist
(async-loop-metrics) -> alist
```

`system-memory-info` contains `total`, `free`, `constrained`, `available`, and
`resident-set`. `system-process-info` contains `pid`, `parent-pid`, and
`available-parallelism`. `system-path-info` contains `executable`,
`current-directory`, `home-directory`, `temporary-directory`, and `hostname`.
`system-uname` contains `system`, `release`, `version`, and `machine`.

Each CPU entry contains `model`, `speed`, `user`, `nice`, `system`, `idle`,
and `irq`. Each interface entry contains `name`, `address`, `netmask`,
`family`, `internal?`, and `physical-address`.

Resource usage contains `user-time`, `system-time`,
`maximum-resident-set-size`, `minor-page-faults`, `major-page-faults`, `swaps`,
`input-blocks`, `output-blocks`, `signals`, `voluntary-context-switches`, and
`involuntary-context-switches`. Time values are `(seconds . microseconds)`
pairs.

`async-loop-metrics` requires an active async scheduler and contains `now`,
`idle-time`, `backend-timeout`, `backend-fd`, `alive?`, `loop-count`, `events`,
and `events-waiting`. High-resolution time and loop idle time are nanoseconds;
uptime is seconds; loop `now` and `backend-timeout` are milliseconds. Memory
quantities are bytes.

### Examples

Generate random bytes and inspect metrics for the scheduler loop that performs
the operation:

```scheme
(import (chezscheme)
        (chezscheme async)
        (chezscheme async io system))

(run-async
  (lambda ()
    (let ([token (random-bytevector 32)]
          [metrics (async-loop-metrics)])
      (list (bytevector-length token)
            (cdr (assq 'loop-count metrics))
            (cdr (assq 'idle-time metrics))))))
;; => (32 loop-count idle-time-in-nanoseconds)
```

System snapshots that do not return operations can be queried directly:

```scheme
(import (chezscheme)
        (chezscheme async io system))

(let ([uname (system-uname)]
      [memory (system-memory-info)])
  (list (cdr (assq 'system uname))
        (cdr (assq 'machine uname))
        (cdr (assq 'total memory))))
;; => (system-name machine-name total-memory-in-bytes)
```
