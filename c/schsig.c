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

/* locally defined functions */
static void split(ptr k, ptr *s);
static void reset_scheme(ptr tc);
static NORETURN void do_error(iptr type, const char *who, const char *s, ptr args);
static void handle_call_error(ptr tc, iptr type, ptr x);
static void init_signal_handlers(void);
static void keyboard_interrupt(ptr tc);

static void (*register_modified_signal)(int);

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

#define compose_memo_inline_size 16
#define compose_scratch_inline_size 64

typedef struct {
    ptr key;
    ptr value;
} compose_memo_entry;

typedef struct {
    compose_memo_entry inline_entries[compose_memo_inline_size];
    uptr inline_count;
    compose_memo_entry *table;
    uptr table_size;
    uptr table_count;
} compose_memo;

typedef struct {
    ptr inline_nodes[compose_scratch_inline_size];
    ptr *nodes;
    uptr capacity;
} compose_scratch;

typedef struct {
    compose_memo attachments;
    compose_memo winders;
    compose_memo winder_lists;
    compose_scratch attachment_scratch;
    compose_scratch winder_scratch;
    ptr old_winders;
    ptr new_winders;
    ptr old_attachments;
    ptr new_attachments;
    ptr winder_rtd1;
    ptr winder_rtd2;
} compose_context;

static void compose_memo_init(compose_memo *memo) {
    memo->inline_count = 0;
    memo->table = NULL;
    memo->table_size = 0;
    memo->table_count = 0;
}

static void compose_memo_table_insert(compose_memo_entry *table,
                                      uptr table_size,
                                      ptr key,
                                      ptr value) {
    uptr i = eq_hash(key) & (table_size - 1);

    while (table[i].key != (ptr)0 && table[i].key != key)
        i = (i + 1) & (table_size - 1);
    table[i].key = key;
    table[i].value = value;
}

static void compose_memo_promote(compose_memo *memo) {
    compose_memo_entry *table;
    uptr i;

    table = calloc(64, sizeof(compose_memo_entry));
    if (table == NULL)
        S_error_abort("compose continuation: calloc failed");
    for (i = 0; i < memo->inline_count; i += 1)
        compose_memo_table_insert(table,
                                  64,
                                  memo->inline_entries[i].key,
                                  memo->inline_entries[i].value);
    memo->table = table;
    memo->table_size = 64;
    memo->table_count = memo->inline_count;
}

static void compose_memo_grow(compose_memo *memo) {
    compose_memo_entry *old_table, *new_table;
    uptr old_size, new_size, i;

    old_table = memo->table;
    old_size = memo->table_size;
    new_size = old_size << 1;
    new_table = calloc(new_size, sizeof(compose_memo_entry));
    if (new_table == NULL)
        S_error_abort("compose continuation: calloc failed");
    for (i = 0; i < old_size; i += 1) {
        if (old_table[i].key != (ptr)0)
            compose_memo_table_insert(new_table,
                                      new_size,
                                      old_table[i].key,
                                      old_table[i].value);
    }
    free(old_table);
    memo->table = new_table;
    memo->table_size = new_size;
}

static ptr compose_memo_ref(compose_memo *memo, ptr key) {
    uptr i;

    if (memo->table == NULL) {
        for (i = 0; i < memo->inline_count; i += 1) {
            if (memo->inline_entries[i].key == key)
                return memo->inline_entries[i].value;
        }
        return (ptr)0;
    }
    i = eq_hash(key) & (memo->table_size - 1);
    while (memo->table[i].key != (ptr)0) {
        if (memo->table[i].key == key)
            return memo->table[i].value;
        i = (i + 1) & (memo->table_size - 1);
    }
    return (ptr)0;
}

static void compose_memo_set(compose_memo *memo, ptr key, ptr value) {
    uptr i;

    if (memo->table == NULL) {
        for (i = 0; i < memo->inline_count; i += 1) {
            if (memo->inline_entries[i].key == key) {
                memo->inline_entries[i].value = value;
                return;
            }
        }
        if (memo->inline_count < compose_memo_inline_size) {
            memo->inline_entries[memo->inline_count].key = key;
            memo->inline_entries[memo->inline_count].value = value;
            memo->inline_count += 1;
            return;
        }
        compose_memo_promote(memo);
    }
    if ((memo->table_count + 1) * 4 >= memo->table_size * 3)
        compose_memo_grow(memo);
    i = eq_hash(key) & (memo->table_size - 1);
    while (memo->table[i].key != (ptr)0) {
        if (memo->table[i].key == key) {
            memo->table[i].value = value;
            return;
        }
        i = (i + 1) & (memo->table_size - 1);
    }
    memo->table[i].key = key;
    memo->table[i].value = value;
    memo->table_count += 1;
}

static void compose_memo_destroy(compose_memo *memo) {
    if (memo->table != NULL)
        free(memo->table);
}

static void compose_scratch_init(compose_scratch *scratch) {
    scratch->nodes = scratch->inline_nodes;
    scratch->capacity = compose_scratch_inline_size;
}

static void compose_scratch_push(compose_scratch *scratch,
                                 uptr count,
                                 ptr node) {
    ptr *nodes;
    uptr capacity;

    if (count == scratch->capacity) {
        capacity = scratch->capacity << 1;
        if (scratch->nodes == scratch->inline_nodes) {
            nodes = malloc(capacity * sizeof(ptr));
            if (nodes != NULL)
                memcpy(nodes, scratch->inline_nodes, count * sizeof(ptr));
        } else {
            nodes = realloc(scratch->nodes, capacity * sizeof(ptr));
        }
        if (nodes == NULL)
            S_error_abort("compose continuation: malloc failed");
        scratch->nodes = nodes;
        scratch->capacity = capacity;
    }
    scratch->nodes[count] = node;
}

static void compose_scratch_destroy(compose_scratch *scratch) {
    if (scratch->nodes != scratch->inline_nodes)
        free(scratch->nodes);
}

static void compose_context_init(compose_context *context,
                                 ptr old_winders,
                                 ptr new_winders,
                                 ptr old_attachments,
                                 ptr new_attachments) {
    compose_memo_init(&context->attachments);
    compose_memo_init(&context->winders);
    compose_memo_init(&context->winder_lists);
    compose_scratch_init(&context->attachment_scratch);
    compose_scratch_init(&context->winder_scratch);
    context->old_winders = old_winders;
    context->new_winders = new_winders;
    context->old_attachments = old_attachments;
    context->new_attachments = new_attachments;
    context->winder_rtd1 = (ptr)0;
    context->winder_rtd2 = (ptr)0;
}

static void compose_context_destroy(compose_context *context) {
    compose_memo_destroy(&context->attachments);
    compose_memo_destroy(&context->winders);
    compose_memo_destroy(&context->winder_lists);
    compose_scratch_destroy(&context->attachment_scratch);
    compose_scratch_destroy(&context->winder_scratch);
}

/* Replace old_tail in ls with new_tail. Preserve sharing among the dynamic
 * state lists in a continuation chain, so each source pair is copied at most
 * once. Source pairs can also be reachable through an independently captured
 * native continuation and must not be modified. */
static ptr compose_continuation_list(compose_context *context,
                                     ptr ls,
                                     ptr old_tail,
                                     ptr new_tail) {
    ptr cached, node, result, scan;
    uptr count = 0;

    if (old_tail == new_tail)
        return ls;
    cached = compose_memo_ref(&context->attachments, ls);
    if (cached != (ptr)0)
        return cached;
    scan = ls;
    for (;;) {
        if (scan == old_tail) {
            result = new_tail;
            break;
        }
        cached = compose_memo_ref(&context->attachments, scan);
        if (cached != (ptr)0) {
            result = cached;
            break;
        }
        /* Some underflow continuations do not record dynamic state. Their
         * empty list is a placeholder, not an extension of the boundary. */
        if (scan == Snil)
            return ls;
        compose_scratch_push(&context->attachment_scratch, count, scan);
        count += 1;
        scan = Scdr(scan);
    }
    while (count != 0) {
        count -= 1;
        node = context->attachment_scratch.nodes[count];
        result = Scons(Scar(node), result);
        compose_memo_set(&context->attachments, node, result);
    }
    return result;
}

/* A winder records the attachments that are current just outside its dynamic
 * extent. Rebase that attachment tail by cloning the winder. Winder fields
 * are in, out, and attachments; critical winders have the same instance
 * fields. */
static ptr compose_continuation_winder(compose_context *context,
                                       ptr w,
                                       ptr old_tail,
                                       ptr new_tail) {
    ptr attachments, cached, copy, rtd;
    iptr size;

    cached = compose_memo_ref(&context->winders, w);
    if (cached != (ptr)0)
        return cached;
    rtd = RECORDINSTTYPE(w);
    if (context->winder_rtd1 == (ptr)0)
        context->winder_rtd1 = rtd;
    else if (rtd != context->winder_rtd1
             && context->winder_rtd2 == (ptr)0)
        context->winder_rtd2 = rtd;
    attachments = compose_continuation_list(context,
                                            RECORDINSTIT(w, 2),
                                            old_tail,
                                            new_tail);
    if (attachments == RECORDINSTIT(w, 2)) {
        compose_memo_set(&context->winders, w, w);
        return w;
    }
    size = UNFIX(RECORDDESCSIZE(rtd));
    copy = S_record(size_record_inst(size));
    RECORDINSTTYPE(copy) = rtd;
    memcpy(&RECORDINSTIT(copy, 0), &RECORDINSTIT(w, 0), size - ptr_bytes);
    RECORDINSTIT(copy, 2) = attachments;
    compose_memo_set(&context->winders, w, copy);
    return copy;
}

static ptr compose_continuation_winders(compose_context *context,
                                        ptr ls,
                                        ptr old_tail,
                                        ptr new_tail,
                                        ptr old_attachments,
                                        ptr new_attachments) {
    ptr cached, node, result, scan, w;
    uptr count = 0;

    if (old_tail == new_tail && old_attachments == new_attachments)
        return ls;
    cached = compose_memo_ref(&context->winder_lists, ls);
    if (cached != (ptr)0)
        return cached;
    scan = ls;
    for (;;) {
        if (scan == old_tail) {
            result = new_tail;
            break;
        }
        cached = compose_memo_ref(&context->winder_lists, scan);
        if (cached != (ptr)0) {
            result = cached;
            break;
        }
        if (scan == Snil)
            return ls;
        compose_scratch_push(&context->winder_scratch, count, scan);
        count += 1;
        scan = Scdr(scan);
    }
    while (count != 0) {
        count -= 1;
        node = context->winder_scratch.nodes[count];
        w = compose_continuation_winder(context,
                                        Scar(node),
                                        old_attachments,
                                        new_attachments);
        result = Scons(w, result);
        compose_memo_set(&context->winder_lists, node, result);
    }
    return result;
}

static ptr compose_continuation_stack_value(compose_context *context, ptr value) {
    ptr mapped, scan, slow;
    uptr steps;

    /* The empty list is also an ordinary Scheme value. Rewriting every live
     * occurrence would turn unrelated user data into the composed dynamic
     * context when the prompt boundary has no winders or attachments. */
    if (context->old_winders != Snil
        && value == context->old_winders)
        return context->new_winders;
    if (context->old_attachments != Snil
        && context->old_attachments != Sfalse
        && value == context->old_attachments)
        return context->new_attachments;
    mapped = compose_memo_ref(&context->winder_lists, value);
    if (mapped != (ptr)0)
        return mapped;
    mapped = compose_memo_ref(&context->winders, value);
    if (mapped != (ptr)0)
        return mapped;
    mapped = compose_memo_ref(&context->attachments, value);
    if (mapped != (ptr)0)
        return mapped;

    /* Critical dynamic-wind frames also keep temporary winder lists, such as
     * a disable-interrupts prefix, that are not installed on a continuation
     * object. Recognize them when they reach the boundary or a mapped suffix. */
    if (!Spairp(value)
        || !Srecordp(Scar(value))
        || (RECORDINSTTYPE(Scar(value)) != context->winder_rtd1
            && RECORDINSTTYPE(Scar(value)) != context->winder_rtd2))
        return value;
    scan = value;
    slow = value;
    steps = 0;
    for (;;) {
        if (scan == context->old_winders
            || compose_memo_ref(&context->winder_lists, scan) != (ptr)0)
            break;
        if (!Spairp(scan))
            return value;
        scan = Scdr(scan);
        steps += 1;
        if ((steps & 1) == 0) {
            if (!Spairp(slow))
                return value;
            slow = Scdr(slow);
            if (scan == slow)
                return value;
        }
    }
    return compose_continuation_winders(context,
                                        value,
                                        context->old_winders,
                                        context->new_winders,
                                        context->old_attachments,
                                        context->new_attachments);
}

/* Rewrite only live Scheme pointers in a copied stack. Compiler-generated
 * dynamic-wind frames keep winder and attachment lists in live slots, in
 * addition to the copies recorded on continuation objects. */
static void compose_continuation_stack(compose_context *context,
                                       ptr stack,
                                       iptr length,
                                       ptr return_address) {
    ptr live_mask;
    ptr *base, *frame, *slot;
    uptr mask;
    iptr index;
    INT bits;
    bigit big_mask;

    base = TO_VOIDP(stack);
    frame = TO_VOIDP((uptr)stack + length);
    while (frame != base) {
        if (frame < base)
            S_error_abort("compose continuation: malformed stack");
        frame = TO_VOIDP((uptr)TO_PTR(frame)
                         - ENTRYFRAMESIZE(return_address));
        live_mask = ENTRYLIVEMASK(return_address);
        slot = frame;
        return_address = *slot;
        if (Sfixnump(live_mask)) {
            mask = UNFIX(live_mask);
            while (mask != 0) {
                slot += 1;
                if (mask & 1)
                    *slot = compose_continuation_stack_value(context, *slot);
                mask >>= 1;
            }
        } else {
            index = BIGLEN(live_mask);
            while (index != 0) {
                index -= 1;
                bits = bigit_bits;
                big_mask = BIGIT(live_mask, index);
                while (bits > 0) {
                    bits -= 1;
                    slot += 1;
                    if (big_mask & 1)
                        *slot = compose_continuation_stack_value(context,
                                                                 *slot);
                    big_mask >>= 1;
                }
            }
        }
    }
}

/* Rebase dynamic-state pointers in an exclusively owned generation-0
 * one-shot stack.  Inactive stack storage is untyped space_data and is not a
 * valid dirty-card target, so callers must copy promoted stacks instead. */
static void compose_continuation_stack_in_place(compose_context *context,
                                                ptr stack,
                                                iptr length,
                                                ptr return_address) {
    ptr live_mask, value, replacement;
    ptr *base, *frame, *slot;
    uptr mask;
    iptr index;
    INT bits;
    bigit big_mask;

    base = TO_VOIDP(stack);
    frame = TO_VOIDP((uptr)stack + length);
    while (frame != base) {
        if (frame < base)
            S_error_abort("compose one-shot continuation: malformed stack");
        frame = TO_VOIDP((uptr)TO_PTR(frame)
                         - ENTRYFRAMESIZE(return_address));
        live_mask = ENTRYLIVEMASK(return_address);
        slot = frame;
        return_address = *slot;
        if (Sfixnump(live_mask)) {
            mask = UNFIX(live_mask);
            while (mask != 0) {
                slot += 1;
                if (mask & 1) {
                    value = *slot;
                    replacement = compose_continuation_stack_value(context,
                                                                    value);
                    if (replacement != value)
                        *slot = replacement;
                }
                mask >>= 1;
            }
        } else {
            index = BIGLEN(live_mask);
            while (index != 0) {
                index -= 1;
                bits = bigit_bits;
                big_mask = BIGIT(live_mask, index);
                while (bits > 0) {
                    bits -= 1;
                    slot += 1;
                    if (big_mask & 1) {
                        value = *slot;
                        replacement = compose_continuation_stack_value(context,
                                                                        value);
                        if (replacement != value)
                            *slot = replacement;
                    }
                    big_mask >>= 1;
                }
            }
        }
    }
}

/* Populate all dynamic-state mappings before rewriting any stack, since a
 * frame can refer to state recorded on another continuation in the segment. */
static void compose_continuation_prepare(compose_context *context,
                                         ptr k,
                                         ptr boundary) {
    ptr source = k;

    while (source != boundary) {
        if (CONTATTACHMENTS(source) != Sfalse) {
            (void)compose_continuation_winders(context,
                                               CONTWINDERS(source),
                                               context->old_winders,
                                               context->new_winders,
                                               context->old_attachments,
                                               context->new_attachments);
            (void)compose_continuation_list(context,
                                            CONTATTACHMENTS(source),
                                            context->old_attachments,
                                            context->new_attachments);
        }
        source = CONTLINK(source);
    }
}

/* Copy the segment from k up to but not including boundary, then splice the
 * copy onto tail using mappings prepared for the complete composition.  Each
 * copied continuation owns its stack storage, since the source and copy can
 * be entered independently.  GC safety: find_room may queue but never run a
 * collection, and collection only happens at Scheme event checks, which
 * cannot occur inside a non-collect-safe foreign procedure. */
static ptr compose_continuation_copy(compose_context *context,
                                     ptr k,
                                     ptr boundary,
                                     ptr tail) {
    ptr head, last, next, source, stack, winders, attachments;
    ptr tc = get_thread_context();
    iptr length;

    head = last = Snil;
    source = k;
    while (source != boundary) {
        length = CONTCLENGTH(source);
        find_room(tc, space_new, 0, type_untyped, length, stack);
        memcpy(TO_VOIDP(stack), TO_VOIDP(CONTSTACK(source)), length);
        if (CONTATTACHMENTS(source) == Sfalse) {
            winders = CONTWINDERS(source);
            attachments = Sfalse;
        } else {
            winders = compose_continuation_winders(context,
                                                   CONTWINDERS(source),
                                                   context->old_winders,
                                                   context->new_winders,
                                                   context->old_attachments,
                                                   context->new_attachments);
            attachments = compose_continuation_list(context,
                                                    CONTATTACHMENTS(source),
                                                    context->old_attachments,
                                                    context->new_attachments);
        }
        compose_continuation_stack(context,
                                   stack,
                                   length,
                                   CONTRET(source));
        next = S_mkcontinuation(space_new,
                                0,
                                CLOSENTRY(source),
                                stack,
                                length,
                                length,
                                tail,
                                CONTRET(source),
                                winders,
                                attachments);
        if (head == Snil)
            head = next;
        else
            CONTLINK(last) = next;
        last = next;
        source = CONTLINK(source);
    }
    return head;
}

static ptr compose_continuation(ptr k,
                                ptr boundary,
                                ptr tail) {
    ptr result;
    compose_context context;

    S_promote_to_multishot(k);
    compose_context_init(&context,
                         CONTWINDERS(boundary),
                         CONTWINDERS(tail),
                         CONTATTACHMENTS(boundary),
                         CONTATTACHMENTS(tail));
    compose_continuation_prepare(&context, k, boundary);
    result = compose_continuation_copy(&context, k, boundary, tail);
    compose_context_destroy(&context);
    return result;
}

ptr S_compose_continuation(ptr k, ptr boundary, ptr tail) {
    return compose_continuation(k, boundary, tail);
}

static IBOOL continuation_oneshot_owned(ptr k) {
    return CONTLENGTH(k) > CONTCLENGTH(k)
           && SegInfo(ptr_get_segment(k))->generation == 0
           && SegInfo(ptr_get_segment(CONTSTACK(k)))->generation == 0;
}

/* Consume the exclusively owned prefix of a one-shot segment by rebasing its
 * dynamic context and replacing its final link in place.  A general one-shot
 * continuation has spare stack capacity (length > clength).  Continuation
 * records below that prefix can be shared with prompt and exception machinery;
 * compose only that suffix, then link the consumed prefix to its copy. */
ptr S_compose_continuation_oneshot(ptr k, ptr boundary, ptr tail) {
    ptr source, next, last, exclusive_limit, composed_tail;
    ptr winders, attachments;
    ptr old_winders, new_winders, old_attachments, new_attachments;
    compose_context context;

    if (k == boundary)
        return tail;

    if (!continuation_oneshot_owned(k))
        return compose_continuation(k, boundary, tail);

    source = k;
    while (source != boundary
           && continuation_oneshot_owned(source))
        source = CONTLINK(source);
    exclusive_limit = source;
    if (source != boundary)
        S_promote_to_multishot(source);

    old_winders = CONTWINDERS(boundary);
    new_winders = CONTWINDERS(tail);
    old_attachments = CONTATTACHMENTS(boundary);
    new_attachments = CONTATTACHMENTS(tail);
    compose_context_init(&context,
                         old_winders,
                         new_winders,
                         old_attachments,
                         new_attachments);
    compose_continuation_prepare(&context, k, boundary);
    composed_tail = exclusive_limit == boundary
                    ? tail
                    : compose_continuation_copy(&context,
                                                exclusive_limit,
                                                boundary,
                                                tail);

    last = Snil;
    source = k;
    while (source != exclusive_limit) {
        next = CONTLINK(source);
        if (CONTATTACHMENTS(source) != Sfalse) {
            winders = compose_continuation_winders(&context,
                                                   CONTWINDERS(source),
                                                   old_winders,
                                                   new_winders,
                                                   old_attachments,
                                                   new_attachments);
            attachments = compose_continuation_list(&context,
                                                    CONTATTACHMENTS(source),
                                                    old_attachments,
                                                    new_attachments);
            CONTWINDERS(source) = winders;
            CONTATTACHMENTS(source) = attachments;
        }
        compose_continuation_stack_in_place(&context,
                                            CONTSTACK(source),
                                            CONTCLENGTH(source),
                                            CONTRET(source));
        last = source;
        source = next;
    }
    CONTLINK(last) = composed_tail;
    compose_context_destroy(&context);
    return k;
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

        S_protect(&S_G.nuate_id);
        S_G.nuate_id = S_intern((const unsigned char *)"$nuate");
        S_set_symbol_value(S_G.nuate_id, FIX(0));

        S_protect(&S_G.null_continuation_id);
        S_G.null_continuation_id = S_intern((const unsigned char *)"$null-continuation");

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
