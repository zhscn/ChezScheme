/* schsig.c
 * Copyright 1984-2017 Cisco Systems, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "system.h"
#include <setjmp.h>
#include <errno.h>
#if defined(PTHREADS) && !defined(WIN32)
# include <sched.h>
#endif

/* locally defined functions */
static void split(ptr k, ptr *s);
static void reset_scheme(ptr tc);
static NORETURN void do_error(iptr type, const char *who, const char *s, ptr args);
static void handle_call_error(ptr tc, iptr type, ptr x);
static void init_signal_handlers(void);
static void keyboard_interrupt(ptr tc);

static void (*register_modified_signal)(int);

/* Native-fiber transition fault injection is dormant unless a debug fiber
   reaches an explicitly armed point. The state contains no Scheme pointers. */
typedef struct {
  iptr point;
  iptr mode;
  iptr action;
  iptr hit;
  iptr release;
  iptr saw_gc;
  iptr exit_requested;
  iptr saw_exit;
} native_fiber_test_state;

enum {
  native_fiber_test_point_install = 1,
  native_fiber_test_point_post_install = 2,
  native_fiber_test_point_commit = 3,
  native_fiber_test_point_post_commit = 4
};

enum {
  native_fiber_test_mode_immediate = 0,
  native_fiber_test_mode_release = 1,
  native_fiber_test_mode_gc = 2,
  native_fiber_test_mode_worker_exit = 3
};

enum {
  native_fiber_test_action_none = 0,
  native_fiber_test_action_source_publication = 1,
  native_fiber_test_action_commit_publication = 2,
  native_fiber_test_action_return_point = 3,
  native_fiber_test_action_stack_overlap = 4,
  native_fiber_test_action_scratch = 5,
  native_fiber_test_action_timer = 6
};

static native_fiber_test_state S_native_fiber_test_state;
#ifdef PTHREADS
static s_thread_mutex_t S_native_fiber_test_mutex;
# define native_fiber_test_lock() \
    ((void)s_thread_mutex_lock(&S_native_fiber_test_mutex))
# define native_fiber_test_unlock() \
    ((void)s_thread_mutex_unlock(&S_native_fiber_test_mutex))
#else
# define native_fiber_test_lock() ((void)0)
# define native_fiber_test_unlock() ((void)0)
#endif

static void native_fiber_test_reset_unlocked(void) {
  memset(&S_native_fiber_test_state, 0,
         sizeof(S_native_fiber_test_state));
  S_native_fiber_test_state.release = 1;
}

void S_native_fiber_test_hook_arm(iptr point, iptr mode, iptr action) {
  if (point < native_fiber_test_point_install
      || point > native_fiber_test_point_post_commit
      || mode < native_fiber_test_mode_immediate
      || mode > native_fiber_test_mode_worker_exit
      || action < native_fiber_test_action_none
      || action > native_fiber_test_action_timer)
    S_error_abort("native-fiber test hook parameters are invalid");
  native_fiber_test_lock();
  if (S_native_fiber_test_state.point != 0
      || !S_native_fiber_test_state.release) {
    native_fiber_test_unlock();
    S_error_abort("native-fiber test hook is already armed");
  }
  memset(&S_native_fiber_test_state, 0,
         sizeof(S_native_fiber_test_state));
  S_native_fiber_test_state.mode = mode;
  S_native_fiber_test_state.action = action;
  S_native_fiber_test_state.release = 0;
  S_native_fiber_test_state.point = point;
  native_fiber_test_unlock();
}

void S_native_fiber_test_hook_release(void) {
  native_fiber_test_lock();
  S_native_fiber_test_state.release = 1;
  native_fiber_test_unlock();
}

iptr S_native_fiber_test_hook_hit(void) {
  iptr hit;
  native_fiber_test_lock();
  hit = S_native_fiber_test_state.hit;
  native_fiber_test_unlock();
  return hit;
}

iptr S_native_fiber_test_hook_saw_gc(void) {
  iptr saw_gc;
  native_fiber_test_lock();
  saw_gc = S_native_fiber_test_state.saw_gc;
  native_fiber_test_unlock();
  return saw_gc;
}

void S_native_fiber_test_hook_request_exit(void) {
  native_fiber_test_lock();
  S_native_fiber_test_state.exit_requested = 1;
  native_fiber_test_unlock();
}

iptr S_native_fiber_test_hook_saw_exit(void) {
  iptr saw_exit;
  native_fiber_test_lock();
  saw_exit = S_native_fiber_test_state.saw_exit;
  native_fiber_test_unlock();
  return saw_exit;
}

void S_native_fiber_test_hook_reset(void) {
  native_fiber_test_lock();
  native_fiber_test_reset_unlocked();
  native_fiber_test_unlock();
}

#ifdef tc_native_fiber_transition_disp
static iptr native_fiber_test_hook(iptr point) {
  iptr mode, action;

  native_fiber_test_lock();
  if (S_native_fiber_test_state.point != point) {
    native_fiber_test_unlock();
    return 0;
  }
  action = S_native_fiber_test_state.action;
  mode = S_native_fiber_test_state.mode;
  S_native_fiber_test_state.hit = point;
  /* One arm describes one transition window. Later resume/finish exchanges
     must not replay the same injected event. */
  S_native_fiber_test_state.point = 0;
  native_fiber_test_unlock();

  while (mode != native_fiber_test_mode_immediate) {
    iptr released;
    native_fiber_test_lock();
    released = S_native_fiber_test_state.release;
    native_fiber_test_unlock();
    if (released) break;
    if (mode == native_fiber_test_mode_gc
        && Sboolean_value(
             S_symbol_racy_value(S_G.collect_request_pending_id))) {
      native_fiber_test_lock();
      S_native_fiber_test_state.saw_gc = 1;
      native_fiber_test_unlock();
      break;
    }
    if (mode == native_fiber_test_mode_worker_exit) {
      iptr exit_requested;
      native_fiber_test_lock();
      exit_requested = S_native_fiber_test_state.exit_requested;
      if (exit_requested) S_native_fiber_test_state.saw_exit = 1;
      native_fiber_test_unlock();
      if (exit_requested) break;
    }
#ifdef PTHREADS
# ifdef WIN32
    Sleep(0);
# else
    sched_yield();
# endif
#else
    break;
#endif
  }
  return action;
}

static void native_fiber_test_request_timer(ptr tc) {
  TIMERTICKS(tc) = FIX(0);
  SOMETHINGPENDING(tc) = Strue;
}

#ifndef tc_native_fiber_test_active_disp
static IBOOL native_fiber_debugp(ptr fiber) {
  ptr flags = NATIVE_FIBER_FIELD(fiber, flags);
  return Sfixnump(flags)
      && (UNFIX(flags) & native_fiber_flag_debug) != 0;
}
#endif

static IBOOL native_fiber_test_activep(ptr tc, ptr source, ptr target) {
#ifdef tc_native_fiber_test_active_disp
  (void)source;
  (void)target;
  return NATIVEFIBERTESTACTIVE(tc) == Strue;
#else
  return native_fiber_debugp(source) || native_fiber_debugp(target);
#endif
}

static void native_fiber_check_stack_disjoint(ptr source, ptr target) {
  ptr a = NATIVE_FIBER_FIELD(source, context);
  ptr b = NATIVE_FIBER_FIELD(target, context);

  if (a != Sfalse) {
    uptr alo = (uptr)CONTSTACK(a), alen = (uptr)CONTLENGTH(a);
    uptr blo = (uptr)CONTSTACK(b), blen = (uptr)CONTLENGTH(b);
    uptr ahi = alo + alen, bhi = blo + blen;
    if (ahi < alo || bhi < blo || (alo < bhi && blo < ahi))
      S_error_abort("native-fiber stack ownership overlaps");
  }
}

#ifdef tc_native_fiber_pinned_head_disp
static void native_fiber_unregister_pinned(ptr tc, ptr fiber) {
  ptr previous = Sfalse;
  ptr node = NATIVEFIBERPINNEDHEAD(tc);

  while (node != Sfalse && node != Snil && node != fiber) {
    previous = node;
    node = NATIVE_FIBER_FIELD(node, pinned_next);
  }
  if (node != fiber)
    S_error_abort("finished native fiber missing from pinned registry");
  node = NATIVE_FIBER_FIELD(fiber, pinned_next);
  if (previous == Sfalse) {
    NATIVEFIBERPINNEDHEAD(tc) = node == Snil ? Sfalse : node;
  } else {
    ptr *field = &NATIVE_FIBER_FIELD(previous, pinned_next);
    S_dirty_mark(field, node);
    *field = node;
  }
  NATIVE_FIBER_FIELD(fiber, pinned_next) = Sfalse;
}
#endif
#endif

#ifdef WIN32
typedef int *(*get_errno_ptr_t)(void);
static get_errno_ptr_t msvcrt_get_errno_ptr;
static get_errno_ptr_t ucrt_get_errno_ptr;
#endif

ptr S_get_scheme_arg(ptr tc, iptr n) {

    if (n <= asm_arg_reg_cnt) return REGARG(tc, n);
    else return FRAME(tc, n - asm_arg_reg_cnt);
}

void S_put_scheme_arg(ptr tc, iptr n, ptr x) {

    if (n <= asm_arg_reg_cnt) REGARG(tc, n) = x;
    else FRAME(tc, n - asm_arg_reg_cnt) = x;
}

void S_promote_to_multishot(ptr k) {
    while (CONTLENGTH(k) != CONTCLENGTH(k)) {
        CONTLENGTH(k) = CONTCLENGTH(k);
        k = CONTLINK(k);
    }
}

/* k must be is a multi-shot continuation, and s (the split point)
 * must be strictly between the base and end of k's stack segment. */
static void split(ptr k, ptr *s) {
    iptr m, n;
    seginfo *si;
    ISPC spc;

  /* set m to size of lower piece, n to size of upper piece */
    m = (uptr)TO_PTR(s) - (uptr)CONTSTACK(k);
    n = CONTCLENGTH(k) - m;

    si = SegInfo(ptr_get_segment(k));
    spc = si->space;
    if (spc != space_new) spc = space_continuation; /* to avoid space_count_pure */

  /* insert a new continuation between k and link(k) */
    CONTLINK(k) = S_mkcontinuation(spc,
                                 si->generation,
                                 CLOSENTRY(k),
                                 CONTSTACK(k),
                                 m, m,
                                 CONTLINK(k),
                                 *s,
                                 Snil,
                                 Sfalse);
    CONTLENGTH(k) = CONTCLENGTH(k) = n;
    CONTSTACK(k) = TO_PTR(s);
    *s = TO_PTR(DOUNDERFLOW);
}

/* We may come in to S_split_and_resize with a multi-shot continuation whose
 * stack segment exceeds the copy bound or is too large to fit along
 * with the return values in the current stack.  We may also come in to
 * S_split_and_resize with a one-shot continuation for which all of the
 * above is true and for which there is insufficient space between the
 * top frame and the end of the stack.  If we have to split a 1-shot, we
 * promote it to multi-shot; doing otherwise is too much trouble.  */
void S_split_and_resize(void) {
    ptr tc = get_thread_context();
    ptr k; iptr value_count; iptr n;

  /* cp = continuation, ac0 = return value count */
    k = CP(tc);
    value_count = (iptr)AC0(tc);

    if (CONTCLENGTH(k) > underflow_limit) {
        iptr frame_size;
        ptr *front_stack_ptr, *end_stack_ptr, *split_point, *guard;

        front_stack_ptr = TO_VOIDP(CONTSTACK(k));
        end_stack_ptr = TO_VOIDP((uptr)TO_PTR(front_stack_ptr) + CONTCLENGTH(k));

        guard = TO_VOIDP((uptr)TO_PTR(end_stack_ptr) - underflow_limit);

      /* set split point to base of top frame */
        frame_size = ENTRYFRAMESIZE(CONTRET(k));
        split_point = TO_VOIDP((uptr)TO_PTR(end_stack_ptr) - frame_size);

      /* split only if we have more than one frame */
        if (split_point != front_stack_ptr) {
          /* walk the stack to set split_point at first frame above guard */
          /* note that first frame may have put us below the guard already */
            for (;;) {
                ptr *p;
                frame_size = ENTRYFRAMESIZE(*split_point);
                p = TO_VOIDP((uptr)TO_PTR(split_point) - frame_size);
                if (p < guard) break;
                split_point = p;
            }

          /* promote to multi-shot if necessary */
            S_promote_to_multishot(k);

          /* split */
            split(k, split_point);
        }
    }

  /* make sure the stack is big enough to hold continuation
   * this is conservative: really need stack-base + clength <= esp
   * and clength + size(values) < stack-size; also, size may include
   * argument register values */
    n = CONTCLENGTH(k) + (value_count * sizeof(ptr)) + stack_slop;
    if (n >= SCHEMESTACKSIZE(tc))
       S_reset_scheme_stack(tc, n);
}

iptr S_continuation_depth(ptr k) {
    iptr n, frame_size; ptr *stack_base, *stack_ptr;

    n = 0;
  /* terminate on shot 1-shot, which could be null_continuation */
    while (CONTLENGTH(k) != scaled_shot_1_shot_flag) {
        stack_base = TO_VOIDP(CONTSTACK(k));
        frame_size = ENTRYFRAMESIZE(CONTRET(k));
        stack_ptr = TO_VOIDP((uptr)TO_PTR(stack_base) + CONTCLENGTH(k));
        for (;;) {
            stack_ptr = TO_VOIDP((uptr)TO_PTR(stack_ptr) - frame_size);
            n += 1;
            if (stack_ptr == stack_base) break;
            frame_size = ENTRYFRAMESIZE(*stack_ptr);
        }
        k = CONTLINK(k);
    }
    return n;
}

ptr S_single_continuation(ptr k, iptr n) {
    iptr frame_size; ptr *stack_base, *stack_top, *stack_ptr;

  /* bug out on shot 1-shots, which could be null_continuation */
    while (CONTLENGTH(k) != scaled_shot_1_shot_flag) {
        stack_base = TO_VOIDP(CONTSTACK(k));
        stack_top = TO_VOIDP((uptr)TO_PTR(stack_base) + CONTCLENGTH(k));
        stack_ptr = stack_top;
        frame_size = ENTRYFRAMESIZE(CONTRET(k));
        for (;;) {
            if (n == 0) {
              /* promote to multi-shot if necessary, even if we don't end
               * up in split, since inspector assumes multi-shot */
                S_promote_to_multishot(k);

                if (stack_ptr != stack_top) {
                    split(k, stack_ptr);
                    k = CONTLINK(k);
                }

                stack_ptr = TO_VOIDP((uptr)TO_PTR(stack_ptr) - frame_size);
                if (stack_ptr != stack_base)
                    split(k, stack_ptr);

                return k;
            } else {
                n -= 1;
                stack_ptr = TO_VOIDP((uptr)TO_PTR(stack_ptr) - frame_size);
                if (stack_ptr == stack_base) break;
                frame_size = ENTRYFRAMESIZE(*stack_ptr);
            }
        }
        k = CONTLINK(k);
    }

    return Sfalse;
}

void S_handle_overflow() {
    ptr tc = get_thread_context();

 /* default frame size is enough */
    S_overflow(tc, 0);
}

void S_handle_overflood() {
    ptr tc = get_thread_context();

 /* xp points to where esp needs to be */
    S_overflow(tc, ((ptr *)TO_VOIDP(XP(tc)) - (ptr *)TO_VOIDP(SFP(tc)))*sizeof(ptr));
}

void S_handle_apply_overflood() {
    ptr tc = get_thread_context();

 /* ac0 contains the argument count for the called procedure */
 /* could reduce request by default frame size and number of arg registers */
 /* the "+ 1" is for the return address slot */
    S_overflow(tc, ((iptr)AC0(tc) + 1) * sizeof(ptr));
}

/* Grow the uniquely owned active stack of a native fiber without turning
 * ordinary call frames into continuation segments.  Explicit continuation
 * capture remains responsible for creating and linking continuation storage.
 * Return false only when the current execution context is not a native
 * fiber.  An unrepresentable fiber stack size is an allocation failure, not
 * a reason to change the fiber's stack-growth model. */
static IBOOL grow_native_fiber_stack(ptr tc, iptr frame_request) {
    iptr signed_old_size;
    uptr old_size, sfp_offset, required_size, new_size, max_iptr;
    ptr old_stack, new_stack;

    if (CURRENTNATIVEFIBER(tc) == Sfalse) return 0;
    if (frame_request < 0) S_error_abort("native-fiber stack request is negative");
    if (((uptr)frame_request & (ptr_bytes - 1)) != 0)
      S_error_abort("native-fiber stack request is misaligned");

    old_stack = SCHEMESTACK(tc);
    signed_old_size = SCHEMESTACKSIZE(tc);
    if (old_stack == (ptr)0
        || ((uptr)old_stack & (ptr_bytes - 1)) != 0
        || signed_old_size < stack_slop
        || ((uptr)signed_old_size & (ptr_bytes - 1)) != 0)
      S_error_abort("native-fiber stack size is invalid before growth");
    old_size = (uptr)signed_old_size;
    sfp_offset = (uptr)SFP(tc) - (uptr)old_stack;
    max_iptr = ((uptr)-1) >> 1;

    if (sfp_offset >= old_size || (sfp_offset & (ptr_bytes - 1)) != 0)
      S_error_abort("native-fiber stack geometry is invalid before growth");

    if ((uptr)frame_request > max_iptr - old_size
        || old_size + (uptr)frame_request
             > max_iptr - (uptr)(one_shot_headroom >> 1)) {
      S_error_abort("native-fiber stack size overflow");
      return 1;
    }

    required_size = old_size + (uptr)frame_request
                  + (uptr)(one_shot_headroom >> 1);
    if (old_size <= max_iptr / 2)
      new_size = old_size * 2;
    else
      new_size = required_size;
    if (new_size < required_size) new_size = required_size;
    if (new_size > max_iptr - (byte_alignment - 1)) {
      S_error_abort("native-fiber stack size overflow");
      return 1;
    }
    new_size = (uptr)ptr_align(new_size);
    if (new_size <= old_size || new_size > max_iptr) {
      S_error_abort("native-fiber stack size overflow");
      return 1;
    }

    S_reset_native_fiber_stack(tc, (iptr)new_size);
    new_stack = SCHEMESTACK(tc);
    if (new_stack == old_stack || SCHEMESTACKSIZE(tc) <= (iptr)old_size)
      S_error_abort("native-fiber stack growth did not allocate distinct storage");

    memcpy(TO_VOIDP(new_stack), TO_VOIDP(old_stack), old_size);
    SFP(tc) = (ptr)((uptr)new_stack + sfp_offset);
    return 1;
}

/* allocates a new stack
 * --the old stack below the sfp is turned into a continuation
 * --the old stack above the sfp is copied to the new stack
 * --return address must be in first frame location
 * --scheme registers are preserved or reset
 * frame_request is how much (in bytes) to increase the default frame size
 */
void S_overflow(ptr tc, iptr frame_request) {
    ptr *sfp;
    iptr above_split_size, sfp_offset;
    ptr *split_point, *guard, *other_guard;
    iptr split_stack_length, split_stack_clength;
    ptr nuate;

    if (grow_native_fiber_stack(tc, frame_request)) return;

    sfp = TO_VOIDP(SFP(tc));
    nuate = SYMVAL(S_G.nuate_id);
    if (!Scodep(nuate)) {
        S_error_abort("overflow: nuate not yet defined");
    }

    guard = TO_VOIDP((uptr)TO_PTR(sfp) - underflow_limit);
  /* leave at least stack_slop headroom in the old stack to reduce the need for return-point overflow checks */
    other_guard = TO_VOIDP((uptr)SCHEMESTACK(tc) + (uptr)SCHEMESTACKSIZE(tc) - (uptr)TO_PTR(stack_slop));
    if ((uptr)TO_PTR(other_guard) < (uptr)TO_PTR(guard)) guard = other_guard;

  /* split only if old stack contains more than underflow_limit bytes */
    if (guard > (ptr *)TO_VOIDP(SCHEMESTACK(tc))) {
        iptr frame_size;

      /* set split point to base of the frame below the current one */
        frame_size = ENTRYFRAMESIZE(*sfp);
        split_point = TO_VOIDP((uptr)TO_PTR(sfp) - frame_size);

      /* split only if we have more than one frame */
        if (split_point != TO_VOIDP(SCHEMESTACK(tc))) {
          /* walk the stack to set split_point at first frame above guard */
          /* note that first frame may have put us below the guard already */
            for (;;) {
                ptr *p;

                frame_size = ENTRYFRAMESIZE(*split_point);
                p = TO_VOIDP((uptr)TO_PTR(split_point) - frame_size);
                if (p < guard) break;
                split_point = p;
            }

            split_stack_clength = (uptr)TO_PTR(split_point) - (uptr)SCHEMESTACK(tc);

          /* promote to multi-shot if current stack is shrimpy */
            if (SCHEMESTACKSIZE(tc) < default_stack_size / 4) {
                split_stack_length = split_stack_clength;
                S_promote_to_multishot(STACKLINK(tc));
            } else {
                split_stack_length = SCHEMESTACKSIZE(tc);
            }

          /* create a continuation */
            STACKLINK(tc) = S_mkcontinuation(space_new,
                                        0,
                                        CODEENTRYPOINT(nuate),
                                        SCHEMESTACK(tc),
                                        split_stack_length,
                                        split_stack_clength,
                                        STACKLINK(tc),
                                        *split_point,
                                        Snil,
                                        Sfalse);

          /* overwrite old return address with dounderflow */
              *split_point = TO_PTR(DOUNDERFLOW);
        }
    } else {
        split_point = TO_VOIDP(SCHEMESTACK(tc));
    }

    above_split_size = SCHEMESTACKSIZE(tc) - ((uptr)TO_PTR(split_point) - (uptr)SCHEMESTACK(tc));

  /* allocate a new stack, retaining same relative sfp */
    sfp_offset = (uptr)TO_PTR(sfp) - (uptr)TO_PTR(split_point);
    S_reset_scheme_stack(tc, above_split_size + frame_request);
    SFP(tc) = (ptr)((uptr)SCHEMESTACK(tc) + sfp_offset);

  /* copy up everything above the split point.  we don't know where the
     current frame ends, so we copy through the end of the old stack */
    {ptr *p, *q; iptr n;
     p = TO_VOIDP(SCHEMESTACK(tc));
     q = split_point;
     for (n = above_split_size; n != 0; n -= sizeof(ptr)) *p++ = *q++;
    }
}

void S_error_abort(const char *s) {
    fprintf(stderr, "%s\n", s);
    S_abnormal_exit();
}

void S_abnormal_exit() {
  S_abnormal_exit_proc();
  fprintf(stderr, "abnormal_exit procedure did not exit\n");
  abort();
}

static void reset_scheme(ptr tc) {
    alloc_mutex_acquire();
   /* eap should always be up-to-date now that we write-through to the tc
      when making any changes to eap when eap is a real register */
    S_scan_dirty(TO_VOIDP(EAP(tc)), TO_VOIDP(REAL_EAP(tc)));
    S_reset_allocation_pointer(tc);
    S_reset_scheme_stack(tc, stack_slop);
    alloc_mutex_release();
    FRAME(tc,0) = TO_PTR(DOUNDERFLOW);
    S_maybe_fire_collector(THREAD_GC(tc));
}

/* error_resets occur with the system in an unknown state,
 * thus we must reset with no opportunity for debugging
 */

void S_error_reset(const char *s) {
    ptr tc = get_thread_context();
    if (!S_errors_to_console && (tc != (ptr)0)) reset_scheme(tc);
    do_error(ERROR_RESET, "", s, Snil);
}

void S_error(const char *who, const char *s) {
    do_error(ERROR_OTHER, who, s, Snil);
}

void S_error1(const char *who, const char *s, ptr x) {
    do_error(ERROR_OTHER, who, s, LIST1(x));
}

void S_error2(const char *who, const char *s, ptr x, ptr y) {
    do_error(ERROR_OTHER, who, s, LIST2(x,y));
}

void S_error3(const char *who, const char *s, ptr x, ptr y, ptr z) {
    do_error(ERROR_OTHER, who, s, LIST3(x,y,z));
}

void S_boot_error(ptr who, ptr msg, ptr args) {
  printf("error caught before error-handing subsystem initialized\n"); 
  printf("who: ");
  S_prin1(who);
  printf("\nmsg: ");
  S_prin1(msg);
  printf("\nargs: ");
  S_prin1(args);
  printf("\n");
  fflush(stdout);
  S_abnormal_exit();
}

static void do_error(iptr type, const char *who, const char *s, ptr args) {
    ptr tc = get_thread_context();

    if (S_errors_to_console || tc == (ptr)0 || CCHAIN(tc) == Snil) {
        if (strlen(who) == 0)
          printf("Error: %s\n", s);
        else
          printf("Error in %s: %s\n", who, s);
        S_prin1(args); putchar('\n');
        fflush(stdout);
        S_abnormal_exit();
    }

    args = Scons(FIX(type),
                 Scons((strlen(who) == 0 ? Sfalse : Sstring_utf8(who,-1)),
                       Scons(Sstring_utf8(s, -1), args)));

#ifdef PTHREADS
    while (S_mutex_is_owner(&S_alloc_mutex) && (S_alloc_mutex_depth > 0)) {
      S_alloc_mutex_depth -= 1;
      S_mutex_release(&S_alloc_mutex);
    }
    while (S_mutex_is_owner(&S_tc_mutex) && (S_tc_mutex_depth > 0)) {
      S_tc_mutex_depth -= 1;
      S_mutex_release(&S_tc_mutex);
    }
#endif /* PTHREADS */

    /* in case error is during fasl read: */
    S_thread_end_code_write(tc, static_generation, 0, NULL, 0);

    TRAP(tc) = (ptr)1;
    AC0(tc) = (ptr)1;
    CP(tc) = S_symbol_value(S_G.error_id);
    S_put_scheme_arg(tc, 1, args);
    LONGJMP(TO_VOIDP(CAAR(CCHAIN(tc))), -1);
}

static void handle_call_error(ptr tc, iptr type, ptr x) {
    ptr p, arg1;
    iptr argcnt;

    argcnt = (iptr)AC0(tc);
    arg1 = argcnt == 0 ? Snil : S_get_scheme_arg(tc, 1);
    p = Scons(FIX(type), Scons(FIX(argcnt), Scons(x, Scons(arg1, Snil))));

    if (S_errors_to_console) {
        printf("Call error: ");
        S_prin1(p); putchar('\n'); fflush(stdout);
        S_abnormal_exit();
    }

    CP(tc) = S_symbol_value(S_G.error_id);
    S_put_scheme_arg(tc, 1, p);
    AC0(tc) = (ptr)(argcnt==0 ? 1 : argcnt);
    TRAP(tc) = (ptr)1;         /* Why is this here? */
}

void S_handle_docall_error() {
    ptr tc = get_thread_context();

    AC0(tc) = (ptr)0;
    handle_call_error(tc, ERROR_CALL_NONPROCEDURE, CP(tc));
}

void S_handle_arg_error() {
    ptr tc = get_thread_context();

    handle_call_error(tc, ERROR_CALL_ARGUMENT_COUNT, CP(tc));
}

void S_handle_nonprocedure_symbol() {
    ptr tc = get_thread_context();
    ptr s;

    s = XP(tc);
    handle_call_error(tc,
                      (SYMVAL(s) == sunbound ?
                                ERROR_CALL_UNBOUND :
                                ERROR_CALL_NONPROCEDURE_SYMBOL),
                      s);
}

void S_handle_values_error(void) {
    ptr tc = get_thread_context();

    handle_call_error(tc, ERROR_VALUES, Sfalse);
}

void S_handle_mvlet_error(void) {
    ptr tc = get_thread_context();

    handle_call_error(tc, ERROR_MVLET, Sfalse);
}

#ifdef WIN32
static int *get_errno_ptr(void) {
  return &errno;
}
#endif

ptr S_save_errno(void) {
    int errno_val;

#ifdef WIN32
    {
      ptr tc = get_thread_context();
      if (CURRENTERRNOSOURCE(tc) == Sfalse) {
	errno_val = errno;
      } else if (Schar_value(STRIT(SYMNAME(CURRENTERRNOSOURCE(tc)), 0)) == 'm' /* msvcrt */) {
	if (!msvcrt_get_errno_ptr) {
	  HMODULE hm;
	  get_errno_ptr_t new_get_errno_ptr = NULL;
	  hm = LoadLibrary("msvcrt.dll");
	  if (hm)
	    new_get_errno_ptr = (get_errno_ptr_t)GetProcAddress(hm, "_errno");
	  if (!new_get_errno_ptr)
	    new_get_errno_ptr = get_errno_ptr;
	  while (msvcrt_get_errno_ptr == NULL) {
	    COMPARE_AND_SWAP_PTR(&msvcrt_get_errno_ptr, NULL, new_get_errno_ptr);
	  }
	}
	errno_val = *(msvcrt_get_errno_ptr());
      } else {
	if (!ucrt_get_errno_ptr) {
	  HMODULE hm;
	  get_errno_ptr_t new_get_errno_ptr = NULL;
	  hm = LoadLibrary("ucrtbase.dll");
	  if (hm)
	    new_get_errno_ptr = (get_errno_ptr_t)GetProcAddress(hm, "_errno");
	  if (!new_get_errno_ptr)
	    new_get_errno_ptr = get_errno_ptr;
	  while (ucrt_get_errno_ptr == NULL) {
	    COMPARE_AND_SWAP_PTR(&ucrt_get_errno_ptr, NULL, new_get_errno_ptr);
	  }
	}
	errno_val = *(ucrt_get_errno_ptr());
      }
    }
#else
    errno_val = errno;
#endif

    return Sinteger(errno_val);
}

#ifdef WIN32
ptr S_save_last_error(void) {
    return Sinteger(GetLastError());
}
#endif

void S_native_fiber_transition() {
    ptr tc = get_thread_context();

#ifdef tc_native_fiber_transition_disp
    if (NATIVEFIBERTRANSITION(tc) != Sfalse) {
      ptr phase = NATIVEFIBERTRANSITION(tc);

      if (phase == FIX(native_fiber_transition_phase_invalid_return))
        S_error_abort("native-fiber target return point is invalid");

      if (phase == FIX(native_fiber_transition_phase_post_install_test)) {
        ptr target = CURRENTNATIVEFIBER(tc);
        ptr source = NATIVE_FIBER_FIELD(target, incoming_source);
        if (native_fiber_test_activep(tc, source, target)) {
          iptr action = native_fiber_test_hook(
            native_fiber_test_point_post_install);
          if (action == native_fiber_test_action_return_point)
            CONTRET(NATIVE_FIBER_FIELD(target, context)) = Sfalse;
          if (action == native_fiber_test_action_timer)
            native_fiber_test_request_timer(tc);
        }
        NATIVEFIBERTRANSITION(tc) = Strue;
        return;
      }

      if (phase == FIX(native_fiber_transition_phase_install)
          || phase == FIX(native_fiber_transition_phase_commit)) {
        ptr source, target, control, old_control;
        ptr *field;
        iptr action = native_fiber_test_action_none;

        target = phase == FIX(native_fiber_transition_phase_install)
                   ? AC1(tc) : CURRENTNATIVEFIBER(tc);
        source = phase == FIX(native_fiber_transition_phase_install)
                   ? CURRENTNATIVEFIBER(tc)
                   : NATIVE_FIBER_FIELD(target, incoming_source);

        /* A migratable target may have been release-published by a different
           worker and claimed by Scheme-generated atomic code. Complete that
           acquire before inspecting its ordinary transaction fields. The
           TSan annotation describes the same edge to the race detector. */
        if (phase == FIX(native_fiber_transition_phase_install)) {
          ACQUIRE_FENCE();
          THREAD_SANITIZER_ACQUIRE(
            &NATIVE_FIBER_FIELD(target, control));
        }

        if (native_fiber_test_activep(tc, source, target))
          action = native_fiber_test_hook(
            phase == FIX(native_fiber_transition_phase_install)
              ? native_fiber_test_point_install
              : native_fiber_test_point_commit);

        if (action == native_fiber_test_action_timer)
          native_fiber_test_request_timer(tc);

        if (phase == FIX(native_fiber_transition_phase_install)) {
          if (action == native_fiber_test_action_stack_overlap)
            CONTSTACK(NATIVE_FIBER_FIELD(target, context)) =
              CONTSTACK(NATIVE_FIBER_FIELD(source, context));
          native_fiber_check_stack_disjoint(source, target);
          control = NATIVE_FIBER_FIELD(source, switch_control);
          field = &NATIVE_FIBER_FIELD(source, control);
          old_control = *field;
          /* Mark the destination card and complete ordinary transaction
             writes before atomically release-publishing the control word. */
          S_dirty_mark(field, control);
          if (action == native_fiber_test_action_source_publication)
            *field = control;
          RELEASE_FENCE();
          THREAD_SANITIZER_RELEASE(field);
          if (!COMPARE_AND_SWAP_PTR(field, TO_VOIDP(old_control),
                                    TO_VOIDP(control)))
            S_error_abort("native-fiber source publication raced");

          control = NATIVE_FIBER_FIELD(target, switch_control);
          field = &NATIVE_FIBER_FIELD(target, control);
          old_control = *field;
          S_dirty_mark(field, control);
          RELEASE_FENCE();
          THREAD_SANITIZER_RELEASE(field);
          if (!COMPARE_AND_SWAP_PTR(field, TO_VOIDP(old_control),
                                    TO_VOIDP(control)))
            S_error_abort("native-fiber target publication raced");

#ifdef tc_native_fiber_claimed_disp
          if (NATIVEFIBERCLAIMED(tc) != target)
            S_error_abort("native-fiber claimed target publication raced");
          NATIVEFIBERCLAIMED(tc) = Sfalse;
          NATIVEFIBERCLAIMEDCONTROL(tc) = Sfalse;
#endif
        } else {
          control = NATIVE_FIBER_FIELD(source, commit_control);
          /* Clear private transaction state before release-publishing the
             stable parked/finished control word. */
          NATIVE_FIBER_FIELD(source, switch_control) = Sfalse;
          NATIVE_FIBER_FIELD(source, commit_control) = Sfalse;
          if (native_fiber_test_activep(tc, source, target)) {
            iptr post_action = native_fiber_test_hook(
              native_fiber_test_point_post_commit);
            if (post_action == native_fiber_test_action_scratch)
              NATIVE_FIBER_FIELD(source, switch_control) = Strue;
            if (post_action == native_fiber_test_action_timer)
              native_fiber_test_request_timer(tc);
          }
          if (NATIVE_FIBER_FIELD(source, switch_control) != Sfalse
              || NATIVE_FIBER_FIELD(source, commit_control) != Sfalse)
            S_error_abort("native-fiber source scratch corrupted before commit");
          field = &NATIVE_FIBER_FIELD(source, control);
          old_control = *field;
          S_dirty_mark(field, control);
          if (action == native_fiber_test_action_commit_publication)
            *field = control;
          RELEASE_FENCE();
          THREAD_SANITIZER_RELEASE(field);
          if (!COMPARE_AND_SWAP_PTR(field, TO_VOIDP(old_control),
                                    TO_VOIDP(control)))
            S_error_abort("native-fiber commit publication raced");
#ifdef tc_native_fiber_pinned_head_disp
          if ((UNFIX(control) & native_fiber_state_mask)
                == native_fiber_state_finished
              && (UNFIX(NATIVE_FIBER_FIELD(source, flags))
                    & native_fiber_flag_pinned) != 0
              && (UNFIX(NATIVE_FIBER_FIELD(source, flags))
                    & native_fiber_flag_scheduler) == 0)
            native_fiber_unregister_pinned(tc, source);
#endif
        }

        NATIVEFIBERTRANSITION(tc) = Strue;
        return;
      }

      S_error_abort("native-fiber transition invariant violated");
    }
#endif

    S_error_abort("native-fiber transition entry is inactive");
}

void S_handle_event_detour() {
    ptr tc = get_thread_context();
    ptr resume_proc = CP(tc);
    ptr resume_args = Snil;
    iptr argcnt, stack_avail, i;

    argcnt = (iptr)AC0(tc);
    stack_avail = (((uptr)ESP(tc) - (uptr)SFP(tc)) >> log2_ptr_bytes) - 1;

    if (argcnt < (stack_avail + asm_arg_reg_cnt)) {
      /* Avoid allocation by passing arguments directly. The compiler
         will only use `detour-event` when the expected number is
         small enough to avoid allocation (unless the function expected
         to allocate a list of arguments, anyway). */
      for (i = argcnt; i > 0; i--)
        S_put_scheme_arg(tc, i+1, S_get_scheme_arg(tc, i));
      S_put_scheme_arg(tc, 1, resume_proc);
      CP(tc) = S_symbol_value(S_G.event_and_resume_id);
      AC0(tc) = (ptr)(argcnt+1);
    } else {
      /* We're assuming that either at least one argument can go in a
         register or stack slop will save us. */
      for (i = argcnt; i > 0; i--)
        resume_args = Scons(S_get_scheme_arg(tc, i), resume_args);
      resume_args = Scons(resume_proc, resume_args);
 
      CP(tc) = S_symbol_value(S_G.event_and_resume_star_id);
      S_put_scheme_arg(tc, 1, resume_args);
      AC0(tc) = (ptr)1;
    }
}

static void keyboard_interrupt(ptr tc) {
  KEYBOARDINTERRUPTPENDING(tc) = Strue;
  SOMETHINGPENDING(tc) = Strue;
}

/* used in printf below
static uptr list_length(ptr ls) {
  uptr i = 0;
  while (ls != Snil) { ls = Scdr(ls); i += 1; }
  return i;
}
*/

void S_fire_collector(void) {
  ptr crp_id = S_G.collect_request_pending_id;

/*  printf("firing collector!\n"); fflush(stdout); */

  if (!Sboolean_value(S_symbol_racy_value(crp_id))) {
    ptr ls;

/*    printf("really firing collector!\n"); fflush(stdout); */

    tc_mutex_acquire();
   /* check again in case some other thread beat us to the punch */
    if (!Sboolean_value(S_symbol_value(crp_id))) {
/* printf("firing collector nthreads = %d\n", list_length(S_threads)); fflush(stdout); */
      S_set_symbol_value(crp_id, Strue);
      for (ls = S_threads; ls != Snil; ls = Scdr(ls))
        SOMETHINGPENDING(THREADTC(Scar(ls))) = Strue;
    }
    tc_mutex_release();
  }
}

void S_noncontinuable_interrupt(void) {
  ptr tc = get_thread_context();
  if (tc != (ptr)0) {
    reset_scheme(tc);
    KEYBOARDINTERRUPTPENDING(tc) = Sfalse;
  }
  do_error(ERROR_NONCONTINUABLE_INTERRUPT,"","",Snil);
}

void Sscheme_register_signal_registerer(void (*registerer)(int)) {
  register_modified_signal = registerer;
}

#ifdef WIN32
ptr S_dequeue_scheme_signals(UNUSED ptr tc) {
  return Snil;
}

ptr S_allocate_scheme_signal_queue() {
  return (ptr)0;
}

void S_register_scheme_signal(UNUSED iptr sig) {
  S_error("register_scheme_signal", "unsupported in this version");
}

/* code courtesy Bob Burger, burgerrg@sagian.com
   We cannot call noncontinuable_interrupt, because we are not allowed
   to perform a longjmp inside a signal handler; instead, we don't
   handle the signal, which will cause the process to terminate.
*/

static BOOL WINAPI handle_signal(DWORD dwCtrlType) {
  switch (dwCtrlType) {
    case CTRL_C_EVENT:
    case CTRL_BREAK_EVENT: {
      /* A new thread is created to handle these signals, so Scheme doesn't know about
         it. Consequently, it interrupts the main thread. */
      ptr tc = TO_PTR(S_G.thread_context);
      if (!THREAD_GC(tc)->during_alloc && Sboolean_value(KEYBOARDINTERRUPTPENDING(tc)))
        return(FALSE);
      keyboard_interrupt(tc);
      return(TRUE);
    }
  }
  return(FALSE);
}

#if defined(_M_ARM64) && !defined(PORTABLE_BYTECODE)
static LONG WINAPI fault_handler(LPEXCEPTION_POINTERS e) {
  if (e->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
    ptr tc = get_thread_context();
    if (tc == (ptr)0) /* not a Scheme thread */
      return EXCEPTION_CONTINUE_SEARCH;
    if (THREAD_GC(tc)->during_alloc)
      S_error_abort("nonrecoverable invalid memory reference");
    else
      S_error_reset("invalid memory reference");
  }
  return EXCEPTION_CONTINUE_SEARCH;
}
#endif

static void init_signal_handlers(void) {
  SetConsoleCtrlHandler(handle_signal, TRUE);
#if defined(_M_ARM64) && !defined(PORTABLE_BYTECODE)
  /* On Arm64, the absence of unwind info means that the `__try`...`__catch`
     in "scheme.c" doesn't get a chance to handle exceptions. */
  AddVectoredExceptionHandler(TRUE, fault_handler);
#endif
}
#else /* WIN32 */

#include <signal.h>

static void handle_signal(INT sig, siginfo_t *si, void *data);
static IBOOL enqueue_scheme_signal(ptr tc, INT sig);
static ptr allocate_scheme_signal_queue(void);
static void forward_signal_to_scheme(INT sig);

#define RESET_SIGNAL {\
    sigset_t set;\
    sigemptyset(&set);\
    sigaddset(&set, sig);\
    sigprocmask(SIG_UNBLOCK,&set,(sigset_t *)0);\
}

/* we buffer up to SIGNALQUEUESIZE - 1 unhandled signals, then start dropping them. */
#define SIGNALQUEUESIZE 64
static IBOOL scheme_signals_registered;

/* we use a simple queue for pending signals.  signals are enqueued only by the
   C signal handler and dequeued only by the Scheme event handler.  since the signal
   handler and event handler run in the same thread, there's no need for locks
   or write barriers. */

struct signal_queue {
  INT head;
  INT tail;
  INT data[SIGNALQUEUESIZE];
};

static IBOOL enqueue_scheme_signal(ptr tc, INT sig) {
  struct signal_queue *queue = TO_VOIDP(SIGNALINTERRUPTQUEUE(tc));
  /* ignore the signal if we failed to allocate the queue */
  if (queue == NULL) return 0;
  INT tail = queue->tail;
  INT next_tail = tail + 1;
  if (next_tail == SIGNALQUEUESIZE) next_tail = 0;
  /* ignore the signal if the queue is full */
  if (next_tail == queue->head) return 0;
  queue->data[tail] = sig;
  queue->tail = next_tail;
  return 1;
}

ptr S_dequeue_scheme_signals(ptr tc) {
  ptr ls = Snil;
  struct signal_queue *queue = TO_VOIDP(SIGNALINTERRUPTQUEUE(tc));
  if (queue == NULL) return ls;
  INT head = queue->head;
  INT tail = queue->tail;
  INT i = tail;
  while (i != head) {
    if (i == 0) i = SIGNALQUEUESIZE;
    i -= 1;
    ls = Scons(Sfixnum(queue->data[i]), ls);
  }
  queue->head = tail;
  return ls;
}

static void forward_signal_to_scheme(INT sig) {
  ptr tc = get_thread_context();

#ifdef PTHREADS
  /* deliver signals to the main thread, only; depending
     on the threads that are running, `tc` might even be NULL */
  if (tc != TO_PTR(S_G.thread_context)) {
    pthread_kill(S_main_thread_id, sig);
    RESET_SIGNAL
    return;
  }
#endif

  if (enqueue_scheme_signal(tc, sig)) {
    SIGNALINTERRUPTPENDING(tc) = Strue;
    SOMETHINGPENDING(tc) = Strue;
  }
  RESET_SIGNAL
}

static ptr allocate_scheme_signal_queue(void) {
  /* silently fail to allocate space for signals if malloc returns NULL */
  struct signal_queue *queue = malloc(sizeof(struct signal_queue));
  if (queue != (struct signal_queue *)0) {
    queue->head = queue->tail = 0;
  }
  return TO_PTR(queue);
}

ptr S_allocate_scheme_signal_queue(void) {
  return scheme_signals_registered ? allocate_scheme_signal_queue() : (ptr)0;
}

void S_register_scheme_signal(iptr sig) {
    struct sigaction act;

    tc_mutex_acquire();
    if (!scheme_signals_registered) {
      ptr ls;
      scheme_signals_registered = 1;
      for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
        SIGNALINTERRUPTQUEUE(THREADTC(Scar(ls))) = S_allocate_scheme_signal_queue();
      }
    }
    tc_mutex_release();

    sigfillset(&act.sa_mask);
    act.sa_flags = 0;
    act.sa_handler = forward_signal_to_scheme;
    sigaction(sig, &act, (struct sigaction *)0);
}

static void handle_signal(INT sig, UNUSED siginfo_t *si, UNUSED void *data) {
/* printf("handle_signal(%d) for tc %x\n", sig, UNFIX(get_thread_context())); fflush(stdout); */
  /* check for particular signals */
    switch (sig) {
        case SIGINT: {
            ptr tc = get_thread_context();
           /* disable keyboard interrupts in subordinate threads until we think
             of something more clever to do with them */
            if (tc == TO_PTR(S_G.thread_context)) {
              if (!THREAD_GC(tc)->during_alloc && Sboolean_value(KEYBOARDINTERRUPTPENDING(tc))) {
               /* this is a no-no, but the only other options are to ignore
                  the signal or to kill the process */
                RESET_SIGNAL
                S_noncontinuable_interrupt();
              }
              keyboard_interrupt(tc);
            }
            RESET_SIGNAL
            break;
        }
#ifdef SIGQUIT
        case SIGQUIT:
            RESET_SIGNAL
            S_abnormal_exit();
	    break;	/* Pacify compilers treating fallthrough warnings as errors */
#endif /* SIGQUIT */
        case SIGILL:
            RESET_SIGNAL
            S_error_reset("illegal instruction");
	    break;	/* Pacify compilers treating fallthrough warnings as errors */
        case SIGFPE:
            RESET_SIGNAL
            S_error_reset("arithmetic overflow");
	    break;	/* Pacify compilers treating fallthrough warnings as errors */
#ifdef SIGBUS
        case SIGBUS:
#endif /* SIGBUS */
        case SIGSEGV: {
            ptr tc = get_thread_context();
            RESET_SIGNAL
            if ((tc == (ptr)0) || THREAD_GC(tc)->during_alloc)
                S_error_abort("nonrecoverable invalid memory reference");
            else
                S_error_reset("invalid memory reference");
            break;
        }
        default:
            RESET_SIGNAL
            S_error_reset("unexpected signal");
	    break;
    }
}

static void no_op_register(UNUSED int sigid) {
}

#define SIGACTION(id, act_p, old_p) (register_modified_signal(id), sigaction(id, act_p, old_p))

static void init_signal_handlers(void) {
    struct sigaction act;

    if (register_modified_signal == NULL)
      register_modified_signal = no_op_register;

    sigemptyset(&act.sa_mask);

  /* drop pending keyboard interrupts */
    act.sa_flags = 0;
    act.sa_handler = SIG_IGN;
    SIGACTION(SIGINT, &act, (struct sigaction *)0);

  /* ignore broken pipe signals */
    act.sa_flags = 0;
    act.sa_handler = SIG_IGN;
    SIGACTION(SIGPIPE, &act, (struct sigaction *)0);

  /* set up to catch SIGINT w/no system call restart */
#ifdef SA_INTERRUPT
    act.sa_flags = SA_INTERRUPT|SA_SIGINFO;
#else
    act.sa_flags = SA_SIGINFO;
#endif /* SA_INTERRUPT */
    act.sa_sigaction = handle_signal;
    SIGACTION(SIGINT, &act, (struct sigaction *)0);
#ifdef BSDI
    siginterrupt(SIGINT, 1);
#endif

  /* set up to catch selected signals */
    act.sa_flags = SA_SIGINFO;
    act.sa_sigaction = handle_signal;
#ifdef SA_RESTART
    act.sa_flags |= SA_RESTART;
#endif /* SA_RESTART */
#ifdef SIGQUIT
    SIGACTION(SIGQUIT, &act, (struct sigaction *)0);
#endif /* SIGQUIT */
    SIGACTION(SIGILL, &act, (struct sigaction *)0);
    SIGACTION(SIGFPE, &act, (struct sigaction *)0);
#ifdef SIGBUS
    SIGACTION(SIGBUS, &act, (struct sigaction *)0);
#endif /* SIGBUS */
    SIGACTION(SIGSEGV, &act, (struct sigaction *)0);
}

#endif /* WIN32 */

void S_schsig_init(void) {
    if (S_boot_time) {
        ptr p;
        ptr tc = get_thread_context();

#ifdef PTHREADS
        s_thread_mutex_init(&S_native_fiber_test_mutex);
#endif
        S_native_fiber_test_hook_reset();

        S_protect(&S_G.nuate_id);
        S_G.nuate_id = S_intern((const unsigned char *)"$nuate");
        S_set_symbol_value(S_G.nuate_id, FIX(0));

        S_protect(&S_G.null_continuation_id);
        S_G.null_continuation_id = S_intern((const unsigned char *)"$null-continuation");

        S_protect(&S_G.native_fiber_context_id);
        S_G.native_fiber_context_id =
          S_intern((const unsigned char *)"native-fiber-context");

        S_protect(&S_G.native_fiber_context_code);
        S_G.native_fiber_context_code = Sfalse;

        S_protect(&S_G.native_fiber_rtd);
        S_G.native_fiber_rtd = Sfalse;

        S_protect(&S_G.collect_request_pending_id);
        S_G.collect_request_pending_id = S_intern((const unsigned char *)"$collect-request-pending");

        S_thread_start_code_write(tc, 0, 0, NULL, 0);
        p = S_code(tc, type_code | (code_flag_continuation << code_flags_offset), 0);
        CODERELOC(p) = S_relocation_table(0);
        CODENAME(p) = Sfalse;
        CODEARITYMASK(p) = FIX(0);
        CODEFREE(p) = 0;
        CODEINFO(p) = Sfalse;
        CODEPINFOS(p) = Snil;
        S_thread_end_code_write(tc, 0, 0, NULL, 0);

        S_set_symbol_value(S_G.null_continuation_id,
            S_mkcontinuation(space_new,
                           0,
                           CODEENTRYPOINT(p),
                           FIX(0),
                           scaled_shot_1_shot_flag, scaled_shot_1_shot_flag,
                           FIX(0),
                           FIX(0),
                           Snil,
                           Snil));

        S_protect(&S_G.error_id);
        S_G.error_id = S_intern((const unsigned char *)"$c-error");

        S_protect(&S_G.event_and_resume_id);
        S_G.event_and_resume_id = S_intern((const unsigned char *)"$event-and-resume");

        S_protect(&S_G.event_and_resume_star_id);
        S_G.event_and_resume_star_id = S_intern((const unsigned char *)"$event-and-resume*");

#ifndef WIN32
        scheme_signals_registered = 0;
#endif
    }


    S_set_symbol_value(S_G.collect_request_pending_id, Sfalse);

    init_signal_handlers();
}
