/* gcwrapper.c
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

/* locally defined functions */
static void segment_tell(uptr seg);
static void check_heap_dirty_msg(char *msg, ptr *x);
static IBOOL dirty_listedp(seginfo *x, IGEN from_g, IGEN to_g);
static void check_dirty_space(ISPC s);
static void check_dirty(void);
static void check_locked_object(ptr p, IBOOL locked, IGEN g, IBOOL aftergc, IGEN mcg);

static IBOOL checkheap_noisy;

#define CHECKHEAP_TEST_MARK_OMIT 1
#define CHECKHEAP_TEST_MARK_EXTRA 2

/* This target is deliberately not registered as a GC root: the extra-mark
 * test needs to retain the source address without making the object live. */
static ptr S_checkheap_test_mark_target;
static iptr S_checkheap_test_mark_action;
static ptr S_checkheap_test_remembered_source;
static iptr S_checkheap_test_remembered_index;

void S_checkheap_test_mark_fault(ptr target, iptr action) {
  if (FIXMEDIATE(target)
      || MaybeSegInfo(ptr_get_segment(target)) == NULL
      || (action != CHECKHEAP_TEST_MARK_OMIT
          && action != CHECKHEAP_TEST_MARK_EXTRA))
    S_error_abort("checkheap mark fault is invalid");
  S_checkheap_test_mark_target = target;
  S_checkheap_test_mark_action = action;
}

/* The source is required to stay in an uncollected generation until the
 * armed generation-zero collection reaches its pre-GC heap check. */
void S_checkheap_test_remembered_fault(ptr source, iptr index) {
  if (!Svectorp(source)
      || index < 0
      || index >= (iptr)Svector_length(source)
      || S_checkheap_test_remembered_source != (ptr)0)
    S_error_abort("checkheap remembered-set fault is invalid");
  S_checkheap_test_remembered_source = source;
  S_checkheap_test_remembered_index = index;
}

/* GC phase coverage is dormant outside explicitly armed runtime tests. The
 * phase mask contains no Scheme pointers and is protected independently of
 * collector locks so a test can observe collections performed by another
 * native thread. */
static iptr S_gc_test_phase_bits;
static iptr S_gc_test_phase_target;
static iptr S_gc_test_phase_action;
static IBOOL S_gc_test_phase_active;
#ifdef PTHREADS
static s_thread_mutex_t S_gc_test_phase_mutex;
# define gc_test_phase_lock() \
    ((void)s_thread_mutex_lock(&S_gc_test_phase_mutex))
# define gc_test_phase_unlock() \
    ((void)s_thread_mutex_unlock(&S_gc_test_phase_mutex))
#else
# define gc_test_phase_lock() ((void)0)
# define gc_test_phase_unlock() ((void)0)
#endif

void S_gc_test_phase_reset(void) {
  gc_test_phase_lock();
  S_gc_test_phase_bits = 0;
  S_gc_test_phase_target = -1;
  S_gc_test_phase_action = GC_TEST_ACTION_NONE;
  gc_test_phase_unlock();
  ATOMIC_STORE_IBOOL(&S_gc_test_phase_active, 1);
}

void S_gc_test_phase_arm(iptr phase, iptr action) {
  if (phase < 0 || phase >= GC_TEST_PHASE_COUNT
      || action != GC_TEST_ACTION_ABORT)
    S_error_abort("gc test phase arm is invalid");
  gc_test_phase_lock();
  S_gc_test_phase_bits = 0;
  S_gc_test_phase_target = phase;
  S_gc_test_phase_action = action;
  gc_test_phase_unlock();
  ATOMIC_STORE_IBOOL(&S_gc_test_phase_active, 1);
}

iptr S_gc_test_phase_mask(void) {
  iptr bits;

  ATOMIC_STORE_IBOOL(&S_gc_test_phase_active, 0);
  gc_test_phase_lock();
  bits = S_gc_test_phase_bits;
  gc_test_phase_unlock();
  return bits;
}

void S_gc_test_note_phase(iptr phase) {
  iptr action = GC_TEST_ACTION_NONE;

  if (!ATOMIC_LOAD_IBOOL(&S_gc_test_phase_active)) return;
  if (phase < 0 || phase >= GC_TEST_PHASE_COUNT)
    S_error_abort("gc test phase is invalid");
  gc_test_phase_lock();
  if (ATOMIC_LOAD_IBOOL(&S_gc_test_phase_active)) {
    S_gc_test_phase_bits |= ((iptr)1 << phase);
    if (S_gc_test_phase_target == phase) {
      action = S_gc_test_phase_action;
      S_gc_test_phase_target = -1;
      S_gc_test_phase_action = GC_TEST_ACTION_NONE;
    }
  }
  gc_test_phase_unlock();
  if (action == GC_TEST_ACTION_ABORT)
    S_error_abort("gc phase fault injected");
}

void S_gc_init(void) {
  IGEN g; INT i;

#ifdef PTHREADS
  if (S_boot_time) s_thread_mutex_init(&S_gc_test_phase_mutex);
#endif
  S_gc_test_phase_bits = 0;
  S_gc_test_phase_target = -1;
  S_gc_test_phase_action = GC_TEST_ACTION_NONE;
  ATOMIC_STORE_IBOOL(&S_gc_test_phase_active, 0);

  ATOMIC_STORE_IBOOL(&S_checkheap, 0); /* 0 for disabled, 1 for enabled */
  S_checkheap_errors = 0; /* count of errors detected by checkheap */
  checkheap_noisy = 0; /* 0 for error output only; 1 for more noisy output */
  S_checkheap_test_mark_target = (ptr)0;
  S_checkheap_test_mark_action = 0;
  S_G.prcgeneration = static_generation;

  if (ATOMIC_LOAD_IBOOL(&S_checkheap)) {
    printf(checkheap_noisy ? "NB: check_heap is enabled and noisy\n" : "NB: check_heap_is_enabled\n");
    fflush(stdout);
  }

#ifndef WIN32
  for (g = 0; g <= static_generation; g++) {
    S_child_processes[g] = Snil;
  }
#endif /* WIN32 */

  if (!S_boot_time) return;

  for (g = 0; g <= static_generation; g++) {
    S_G.guardians[g] = Snil;
    S_G.locked_objects[g] = Snil;
    S_G.unlocked_objects[g] = Snil;
  }
  S_G.max_nonstatic_generation = 
    S_G.new_max_nonstatic_generation = 
      S_G.min_free_gen = 
        S_G.new_min_free_gen =
          S_G.min_mark_gen = default_max_nonstatic_generation;

  for (g = 0; g <= static_generation; g += 1) {
    for (i = 0; i < countof_types; i += 1) {
      S_G.countof[g][i] = 0;
      S_G.bytesof[g][i] = 0;
    }
    S_G.gctimestamp[g] = 0;
    S_G.rtds_with_counts[g] = Snil;
  }

  S_G.countof[static_generation][countof_oblist] += 1;
  S_G.bytesof[static_generation][countof_oblist] += S_G.oblist_length * sizeof(bucket *);

  S_protect(&S_G.static_id);
  S_G.static_id = S_intern((const unsigned char *)"static");

  S_protect(&S_G.countof_names);
  S_G.countof_names = S_vector(countof_types);
  for (i = 0; i < countof_types; i += 1) {
    INITVECTIT(S_G.countof_names, i) = FIX(0);
    S_G.countof_size[i] = 0;
  }
  INITVECTIT(S_G.countof_names, countof_pair) = S_intern((const unsigned char *)"pair");
    S_G.countof_size[countof_pair] = size_pair;
  INITVECTIT(S_G.countof_names, countof_symbol) = S_intern((const unsigned char *)"symbol");
    S_G.countof_size[countof_symbol] = size_symbol;
  INITVECTIT(S_G.countof_names, countof_flonum) = S_intern((const unsigned char *)"flonum");
    S_G.countof_size[countof_flonum] = size_flonum;
  INITVECTIT(S_G.countof_names, countof_closure) = S_intern((const unsigned char *)"procedure");
    S_G.countof_size[countof_closure] = 0;
  INITVECTIT(S_G.countof_names, countof_continuation) = S_intern((const unsigned char *)"continuation");
    S_G.countof_size[countof_continuation] = size_continuation;
  INITVECTIT(S_G.countof_names, countof_bignum) = S_intern((const unsigned char *)"bignum");
    S_G.countof_size[countof_bignum] = 0;
  INITVECTIT(S_G.countof_names, countof_ratnum) = S_intern((const unsigned char *)"ratnum");
    S_G.countof_size[countof_ratnum] = size_ratnum;
  INITVECTIT(S_G.countof_names, countof_inexactnum) = S_intern((const unsigned char *)"inexactnum");
    S_G.countof_size[countof_inexactnum] = size_inexactnum;
  INITVECTIT(S_G.countof_names, countof_exactnum) = S_intern((const unsigned char *)"exactnum");
    S_G.countof_size[countof_exactnum] = size_exactnum;
  INITVECTIT(S_G.countof_names, countof_box) = S_intern((const unsigned char *)"box");
    S_G.countof_size[countof_box] = size_box;
  INITVECTIT(S_G.countof_names, countof_port) = S_intern((const unsigned char *)"port");
    S_G.countof_size[countof_port] = size_port;
  INITVECTIT(S_G.countof_names, countof_code) = S_intern((const unsigned char *)"code");
    S_G.countof_size[countof_code] = 0;
  INITVECTIT(S_G.countof_names, countof_thread) = S_intern((const unsigned char *)"thread");
    S_G.countof_size[countof_thread] = size_thread;
  INITVECTIT(S_G.countof_names, countof_tlc) = S_intern((const unsigned char *)"tlc");
    S_G.countof_size[countof_tlc] = size_tlc;
  INITVECTIT(S_G.countof_names, countof_rtd_counts) = S_intern((const unsigned char *)"rtd-counts");
    S_G.countof_size[countof_rtd_counts] = size_rtd_counts;
  INITVECTIT(S_G.countof_names, countof_stack) = S_intern((const unsigned char *)"stack");
    S_G.countof_size[countof_stack] = 0;
  INITVECTIT(S_G.countof_names, countof_relocation_table) = S_intern((const unsigned char *)"reloc-table");
    S_G.countof_size[countof_relocation_table] = 0;
  INITVECTIT(S_G.countof_names, countof_weakpair) = S_intern((const unsigned char *)"weakpair");
    S_G.countof_size[countof_weakpair] = size_pair;
  INITVECTIT(S_G.countof_names, countof_vector) = S_intern((const unsigned char *)"vector");
    S_G.countof_size[countof_vector] = 0;
  INITVECTIT(S_G.countof_names, countof_string) = S_intern((const unsigned char *)"string");
    S_G.countof_size[countof_string] = 0;
  INITVECTIT(S_G.countof_names, countof_fxvector) = S_intern((const unsigned char *)"fxvector");
    S_G.countof_size[countof_fxvector] = 0;
  INITVECTIT(S_G.countof_names, countof_flvector) = S_intern((const unsigned char *)"flvector");
    S_G.countof_size[countof_flvector] = 0;
  INITVECTIT(S_G.countof_names, countof_bytevector) = S_intern((const unsigned char *)"bytevector");
    S_G.countof_size[countof_bytevector] = 0;
  INITVECTIT(S_G.countof_names, countof_locked) = S_intern((const unsigned char *)"locked");
    S_G.countof_size[countof_locked] = 0;
  INITVECTIT(S_G.countof_names, countof_guardian) = S_intern((const unsigned char *)"guardian");
    S_G.countof_size[countof_guardian] = size_guardian_entry;
  INITVECTIT(S_G.countof_names, countof_oblist) = S_intern((const unsigned char *)"oblist");
    S_G.countof_size[countof_guardian] = 0;
  INITVECTIT(S_G.countof_names, countof_ephemeron) = S_intern((const unsigned char *)"ephemeron");
    S_G.countof_size[countof_ephemeron] = size_ephemeron;
  INITVECTIT(S_G.countof_names, countof_stencil_vector) = S_intern((const unsigned char *)"stencil-vector");
    S_G.countof_size[countof_stencil_vector] = 0;
  INITVECTIT(S_G.countof_names, countof_record) = S_intern((const unsigned char *)"record");
    S_G.countof_size[countof_record] = 0;
  INITVECTIT(S_G.countof_names, countof_phantom) = S_intern((const unsigned char *)"phantom");
    S_G.countof_size[countof_phantom] = 0;
  for (i = 0; i < countof_types; i += 1) {
    if (Svector_ref(S_G.countof_names, i) == FIX(0)) {
      fprintf(stderr, "uninitialized countof_name at index %d\n", i);
      S_abnormal_exit();
    }
  }
}

IGEN S_maxgen(void) {
  return S_G.new_max_nonstatic_generation;
}

void S_set_maxgen(IGEN g) {
  if (g < 0 || g >= static_generation) {
    fprintf(stderr, "invalid maxgen %d\n", g);
    S_abnormal_exit();
  }
  if (S_G.new_min_free_gen == S_G.new_max_nonstatic_generation || S_G.new_min_free_gen > g) {
    S_G.new_min_free_gen = g;
  }
  S_G.new_max_nonstatic_generation = g;
}

IGEN S_minfreegen(void) {
  return S_G.new_min_free_gen;
}

void S_set_minfreegen(IGEN g) {
  S_G.new_min_free_gen = g;
  if (S_G.new_max_nonstatic_generation == S_G.max_nonstatic_generation) {
    S_G.min_free_gen = g;
  }
}

IGEN S_minmarkgen(void) {
  return S_G.min_mark_gen;
}

void S_set_minmarkgen(IGEN g) {
  S_G.min_mark_gen = g;
}

void S_immobilize_object(ptr x) {
  seginfo *si;

  if (FIXMEDIATE(x))
    si = NULL;
  else
    si = MaybeSegInfo(ptr_get_segment(x));
 
  if ((si != NULL) && (si->generation != static_generation)) {
    tc_mutex_acquire();

    /* Try a little to to support cancellation of segment-level
     * immobilzation --- but we don't try too hard */
    if (si->must_mark < MUST_MARK_INFINITY) {
      si->must_mark++;
      if (si->generation == 0)
        S_G.must_mark_gen0 = 1;
    }

    /* Note: for `space_new`, `must_mark` doesn't really mean all
       objects must be marked; only those in the locked list must be
       marked. Non-locked objects on `space_new` cannot be immobilized. */

    tc_mutex_release();
  }
}

void S_mobilize_object(ptr x) {
  seginfo *si;

  if (FIXMEDIATE(x))
    si = NULL;
  else
    si = MaybeSegInfo(ptr_get_segment(x));

  if ((si != NULL) && (si->generation != static_generation)) {
    tc_mutex_acquire();

    if (si->must_mark == 0)
      S_error_abort("S_mobilize_object(): object was definitely not immobilzed");

    /* See S_immobilize_object() about this vague try at canceling immobilation: */
    if (si->must_mark < MUST_MARK_INFINITY)
      --si->must_mark;
  
    tc_mutex_release();
  }
}

static IBOOL memqp(ptr x, ptr ls) {
  for (;;) {
    if (ls == Snil) return 0;
    if (Scar(ls) == x) return 1;
    ls = Scdr(ls);
  }
}

static IBOOL remove_first_nomorep(ptr x, ptr *pls, IBOOL look) {
  ptr ls;

  for (;;) {
    ls = *pls;
    if (ls == Snil) break;
    if (Scar(ls) == x) {
      ls = Scdr(ls);
      *pls = ls;
      if (look) return !memqp(x, ls);
      break;
    }
    pls = &Scdr(ls);
  }

 /* must return 0 if we don't look for more */
  return 0;
}

IBOOL Slocked_objectp(ptr x) {
  seginfo *si; IGEN g; IBOOL ans; ptr ls;

  if (FIXMEDIATE(x) || (si = MaybeSegInfo(ptr_get_segment(x))) == NULL || (g = si->generation) == static_generation) return 1;

  tc_mutex_acquire();

  ans = 0;
  for (ls = S_G.locked_objects[g]; ls != Snil; ls = Scdr(ls)) {
    if (x == Scar(ls)) {
      ans = 1;
      break;
    }
  }

  tc_mutex_release();

  return ans;
}

ptr S_locked_objects(void) {
  IGEN g; ptr ans; ptr ls;

  tc_mutex_acquire();

  ans = Snil;
  for (g = 0; g <= static_generation; INCRGEN(g)) {
    for (ls = S_G.locked_objects[g]; ls != Snil; ls = Scdr(ls)) {
      ans = Scons(Scar(ls), ans);
    }
  }

  tc_mutex_release();

  return ans;
}

void Slock_object(ptr x) {
  seginfo *si; IGEN g;

 /* weed out pointers that won't be relocated */
  if (!FIXMEDIATE(x) && (si = MaybeSegInfo(ptr_get_segment(x))) != NULL && (g = si->generation) != static_generation) {
    ptr tc = get_thread_context();
    tc_mutex_acquire();
    THREAD_GC(tc)->during_alloc += 1;
    /* immobilize */
    if (si->must_mark < MUST_MARK_INFINITY) {
      si->must_mark++;
      if (si->generation == 0)
        S_G.must_mark_gen0 = 1;
    }
   /* add x to locked list. remove from unlocked list */
    S_G.locked_objects[g] = S_cons_in(tc, (g == 0 ? space_new : space_impure), g, x, S_G.locked_objects[g]);
    if (S_G.enable_object_counts) {
      if (g != 0) S_G.countof[g][countof_pair] += 1;
    }
    (void)remove_first_nomorep(x, &S_G.unlocked_objects[g], 0);
    THREAD_GC(tc)->during_alloc -= 1;
    tc_mutex_release();
  }
}

void Sunlock_object(ptr x) {
  seginfo *si; IGEN g;

  if (!FIXMEDIATE(x) && (si = MaybeSegInfo(ptr_get_segment(x))) != NULL && (g = si->generation) != static_generation) {
    ptr tc = get_thread_context();
    tc_mutex_acquire();
    THREAD_GC(tc)->during_alloc += 1;
    /* mobilize, if we haven't lost track */
    if (si->must_mark < MUST_MARK_INFINITY)
      --si->must_mark;
   /* remove first occurrence of x from locked list. if there are no
      others, add x to unlocked list */
    if (remove_first_nomorep(x, &S_G.locked_objects[g], (si->space == space_new) && (si->generation > 0))) {
      S_G.unlocked_objects[g] = S_cons_in(tc, (g == 0 ? space_new : space_impure), g, x, S_G.unlocked_objects[g]);
      if (S_G.enable_object_counts) {
        if (g != 0) S_G.countof[g][countof_pair] += 1;
      }
    }
    THREAD_GC(tc)->during_alloc -= 1;
    tc_mutex_release();
  }
}

ptr s_help_unregister_guardian(ptr *pls, ptr tconc, ptr result) {
  ptr rep, ls;
  while ((ls = *pls) != Snil) {
    if (GUARDIANTCONC(ls) == tconc) {
      result = Scons(((rep = GUARDIANREP(ls)) == ftype_guardian_rep ? GUARDIANOBJ(ls) : rep), result);
      *pls = ls = GUARDIANNEXT(ls);
    } else {
      ls = *(pls = &GUARDIANNEXT(ls));
    }
  }
  return result;
}

ptr S_unregister_guardian(ptr tconc) {
  ptr result, tc; IGEN g;
  tc_mutex_acquire();
  tc = get_thread_context();
  /* in the interest of thread safety, gather entries only in the current thread, ignoring any others */
  result = s_help_unregister_guardian(&GUARDIANENTRIES(tc), tconc, Snil);
  /* plus, of course, any already known to the storage-management system */
  for (g = 0; g <= static_generation; INCRGEN(g)) {
    result = s_help_unregister_guardian(&S_G.guardians[g], tconc, result);
  }
  tc_mutex_release();
  return result;
}

#ifndef WIN32
void S_register_child_process(INT child) {
  tc_mutex_acquire();
  S_child_processes[0] = Scons(FIX(child), S_child_processes[0]);
  tc_mutex_release();
}
#endif /* WIN32 */

IBOOL S_enable_object_counts(void) {
  return S_G.enable_object_counts;
}

void S_set_enable_object_counts(IBOOL eoc) {
  S_G.enable_object_counts = eoc;
}

ptr S_object_counts(void) {
  IGEN grtd, g; ptr ls; iptr i; ptr outer_alist;

  tc_mutex_acquire();

  outer_alist = Snil;

 /* add rtds w/nonozero counts to the alist */
  for (grtd = 0; grtd <= static_generation; INCRGEN(grtd)) {
    for (ls = S_G.rtds_with_counts[grtd]; ls != Snil; ls = Scdr(ls)) {
      ptr rtd = Scar(ls);
      ptr counts = RECORDDESCCOUNTS(rtd);
      IGEN g;
      uptr size = size_record_inst(UNFIX(RECORDDESCSIZE(rtd)));
      ptr inner_alist = Snil;

      S_fixup_counts(counts);
      for (g = 0; g <= static_generation; INCRGEN(g)) {
        uptr count = RTDCOUNTSIT(counts, g); IGEN gcurrent = g;
        if (g == S_G.new_max_nonstatic_generation) {
          while (g < S_G.max_nonstatic_generation) {
            g += 1;
            count += RTDCOUNTSIT(counts, g);
          }
        }
        if (count != 0) inner_alist = Scons(Scons((gcurrent == static_generation ? S_G.static_id : FIX(gcurrent)), Scons(Sunsigned(count), Sunsigned(count * size))), inner_alist);
      }
      if (inner_alist != Snil) outer_alist = Scons(Scons(rtd, inner_alist), outer_alist);
    }
  }

 /* add primary types w/nonozero counts to the alist */
  for (i = 0 ; i < countof_types; i += 1) {
    if (i != countof_record) { /* covered by rtd-specific counts */
      ptr inner_alist = Snil;
      for (g = 0; g <= static_generation; INCRGEN(g)) {
        IGEN gcurrent = g;
        uptr count = S_G.countof[g][i];
        uptr bytes = S_G.bytesof[g][i];

        if (g == S_G.new_max_nonstatic_generation) {
          while (g < S_G.max_nonstatic_generation) {
            g += 1;
            /* NB: S_G.max_nonstatic_generation + 1 <= static_generation, but coverity complains about overrun */
            /* coverity[overrun-buffer-val] */
            count += S_G.countof[g][i];
            /* coverity[overrun-buffer-val] */
            bytes += S_G.bytesof[g][i];
          }
        }

        if (count != 0) {
          if (bytes == 0) bytes = count * S_G.countof_size[i];
          inner_alist = Scons(Scons((gcurrent == static_generation ? S_G.static_id : FIX(gcurrent)), Scons(Sunsigned(count), Sunsigned(bytes))), inner_alist);
        }
      }
      if (inner_alist != Snil) outer_alist = Scons(Scons(Svector_ref(S_G.countof_names, i), inner_alist), outer_alist);
    }
  }

  tc_mutex_release();

  return outer_alist;
}

IBOOL S_enable_object_backreferences(void) {
  return S_G.enable_object_backreferences;
}

void S_set_enable_object_backreferences(IBOOL eoc) {
  S_G.enable_object_backreferences = eoc;
}

ptr S_object_backreferences(void) {
  IGEN g; ptr ls = Snil;

  tc_mutex_acquire();

  for (g = S_G.max_nonstatic_generation+1; g--; )
    ls = Scons(S_G.gcbackreference[g], ls);

  tc_mutex_release();

  return ls;
}

seginfo *S_ptr_seginfo(ptr p) {
  return MaybeSegInfo(ptr_get_segment(p)); 
}

/* Scompact_heap().  Compact into as few O/S chunks as possible and
 * move objects into static generation
 */
void Scompact_heap(void) {
  ptr tc = get_thread_context();
  IBOOL eoc = S_G.enable_object_counts;
  THREAD_GC(tc)->during_alloc += 1;
  S_G.enable_object_counts = 1;
  S_gc_oce(tc, S_G.max_nonstatic_generation, static_generation, static_generation, Sfalse);
  S_G.enable_object_counts = eoc;
  THREAD_GC(tc)->during_alloc -= 1;
}

/* S_check_heap checks for various kinds of heap consistency
   It currently checks for:
       dangling references in space_impure (generation > 0) and space_pure
       dirty-card coverage of every old-to-young pointer
       dirty-segment minima and list membership
       a full-heap remembered-set rebuild at each collection boundary

   Some additional things it should check for but doesn't:
       dangling references in space_code and space_continuation
       dirty bits set for non-impure segments outside of generation zero
       proper chaining of segments of a space and generation:
          chains contain all and only the appropriate segments

   If noisy is nonzero, additional comments may be included in the output
*/

#ifdef __MINGW32__
# include <inttypes.h>
# define PHtx "%" PRIxPTR
# define Ptd "%" PRIdPTR
#else
# define PHtx "%#tx"
# define Ptd "%td"
#endif

static void segment_tell(uptr seg) {
  seginfo *si;
  ISPC s, s1;
  static char *spacename[max_space+1] = { alloc_space_names };

  printf("segment "PHtx"", (ptrdiff_t)seg);
  if ((si = MaybeSegInfo(seg)) == NULL) {
    printf(" out of heap bounds\n");
  } else {
    printf(" si=%p", si);
    printf(" generation=%d", si->generation);
    s = si->space;
    s1 = si->space;
    if (s1 < 0 || s1 > max_space)
      printf(" space-bogus (%d)", s);
    else {
      printf(" space-%s", spacename[s1]);
      if (si->old_space) printf(" oldspace");
      if (si->must_mark) printf(" mustmark");
      if (si->marked_mask) printf(" marked");
    }
    printf("\n");
  }
  fflush(stdout);
}

void S_ptr_tell(ptr p) {
  segment_tell(ptr_get_segment(p));
}

void S_addr_tell(ptr p) {
  segment_tell(addr_get_segment(p));
}

static int maybe_inexactnum_marked(ptr p, seginfo *psi) {
  /* test for possible flonum within marked inexactnum */
  uptr real_delta = (uptr)TO_PTR(&INEXACTNUM_REAL_PART(TYPE((ptr)0, type_typed_object)));
  uptr imag_delta = (uptr)TO_PTR(&INEXACTNUM_IMAG_PART(TYPE((ptr)0, type_typed_object)));
  ptr maybe_pr = TYPE((uptr)UNTYPE(p, type_flonum) - real_delta, type_typed_object);
  ptr maybe_pi = TYPE((uptr)UNTYPE(p, type_flonum) - imag_delta, type_typed_object);

  if ((MaybeSegInfo(ptr_get_segment(maybe_pr)) == psi)
      && (psi->marked_mask[segment_bitmap_byte(maybe_pr)] & segment_bitmap_bit(maybe_pr))) {
    return 1;
  }

  if ((MaybeSegInfo(ptr_get_segment(maybe_pi)) == psi)
      && (psi->marked_mask[segment_bitmap_byte(maybe_pi)] & segment_bitmap_bit(maybe_pi))) {
    return 1;
  }

  return 0;
}

static void shadow_seed_checked_pointer(ptr *pp, ptr p, uptr seg, ISPC s);
static uptr size_object(ptr p);

#define REMEMBERED_EMPTY_SEGMENT ((uptr)-1)

typedef struct {
  uptr segment;
  IGEN generation;
  ISPC space;
  unsigned char required[cards_per_segment];
  unsigned char scanned[cards_per_segment];
  unsigned int listed_minimum;
  unsigned int list_count;
} rebuilt_remembered_segment;

typedef struct {
  rebuilt_remembered_segment *segments;
  size_t capacity;
  size_t count;
  size_t occupied_segment_count;
  IBOOL active;
} rebuilt_remembered_state;

static rebuilt_remembered_state rebuilt_remembered;

static IBOOL remembered_spacep(ISPC s) {
  return s == space_impure
      || s == space_symbol
      || s == space_port
      || s == space_impure_record
      || s == space_impure_typed_object
      || s == space_immobile_impure
      || s == space_count_impure
      || s == space_closure
      || s == space_weakpair
      || s == space_ephemeron
      || s == space_reference_array;
}

static IBOOL remembered_object_card_spacep(ISPC s) {
  return s == space_symbol
      || s == space_port
      || s == space_impure_record;
}

static size_t remembered_segment_hash(uptr segment, size_t capacity) {
  segment ^= segment >> 17;
  segment *= (uptr)0xed5ad4bbU;
  segment ^= segment >> 11;
  return (size_t)segment & (capacity - 1);
}

static void remembered_rehash(size_t capacity) {
  rebuilt_remembered_segment *old_segments = rebuilt_remembered.segments;
  size_t old_capacity = rebuilt_remembered.capacity;
  size_t i;

  rebuilt_remembered.segments = (rebuilt_remembered_segment *)malloc(
    capacity * sizeof(rebuilt_remembered_segment));
  if (rebuilt_remembered.segments == NULL)
    S_error_abort("check_heap: cannot allocate remembered-set rebuild");
  rebuilt_remembered.capacity = capacity;
  rebuilt_remembered.count = 0;
  for (i = 0; i < capacity; i += 1)
    rebuilt_remembered.segments[i].segment = REMEMBERED_EMPTY_SEGMENT;

  if (old_segments != NULL) {
    for (i = 0; i < old_capacity; i += 1) {
      if (old_segments[i].segment != REMEMBERED_EMPTY_SEGMENT) {
        size_t at = remembered_segment_hash(old_segments[i].segment,
                                             capacity);
        while (rebuilt_remembered.segments[at].segment
               != REMEMBERED_EMPTY_SEGMENT)
          at = (at + 1) & (capacity - 1);
        rebuilt_remembered.segments[at] = old_segments[i];
        rebuilt_remembered.count += 1;
      }
    }
    free(old_segments);
  }
}

static rebuilt_remembered_segment *remembered_find_segment(uptr segment,
                                                            IBOOL create) {
  size_t at;
  seginfo *si;

  if (rebuilt_remembered.capacity == 0) {
    if (!create) return NULL;
    remembered_rehash(64);
  } else if (create
             && (rebuilt_remembered.count + 1) * 2
                  >= rebuilt_remembered.capacity) {
    remembered_rehash(rebuilt_remembered.capacity * 2);
  }

  at = remembered_segment_hash(segment, rebuilt_remembered.capacity);
  while (rebuilt_remembered.segments[at].segment
         != REMEMBERED_EMPTY_SEGMENT) {
    if (rebuilt_remembered.segments[at].segment == segment)
      return &rebuilt_remembered.segments[at];
    at = (at + 1) & (rebuilt_remembered.capacity - 1);
  }

  if (!create) return NULL;
  si = MaybeSegInfo(segment);
  if (si == NULL || !remembered_spacep(si->space))
    S_error_abort("check_heap: invalid remembered-set source segment");
  rebuilt_remembered.segments[at].segment = segment;
  rebuilt_remembered.segments[at].generation = si->generation;
  rebuilt_remembered.segments[at].space = si->space;
  memset(rebuilt_remembered.segments[at].required, 0xff,
         sizeof(rebuilt_remembered.segments[at].required));
  memset(rebuilt_remembered.segments[at].scanned, 0xff,
         sizeof(rebuilt_remembered.segments[at].scanned));
  rebuilt_remembered.segments[at].listed_minimum = 0xff;
  rebuilt_remembered.segments[at].list_count = 0;
  rebuilt_remembered.count += 1;
  return &rebuilt_remembered.segments[at];
}

static void remembered_reset(void) {
  free(rebuilt_remembered.segments);
  memset(&rebuilt_remembered, 0, sizeof(rebuilt_remembered));
}

static void remembered_begin(IBOOL aftergc, IGEN mcg) {
  IGEN g;
  ISPC s;

  remembered_reset();
  (void)aftergc;
  (void)mcg;
  rebuilt_remembered.active = 1;

  for (g = 0; g <= static_generation; INCRGEN(g))
    for (s = 0; s <= max_real_space; s += 1) {
      seginfo *si;
      for (si = S_G.occupied_segments[g][s]; si != NULL; si = si->next) {
        rebuilt_remembered.occupied_segment_count += 1;
        if (remembered_spacep(s))
          (void)remembered_find_segment(si->number, 1);
      }
    }
}

static void remembered_lower(unsigned char *slot, IGEN generation) {
  if (generation < *slot) *slot = generation;
}

static void remembered_record_pointer(ptr *pp, ptr p,
                                      IBOOL address_is_meaningful,
                                      ptr base) {
  seginfo *from_si, *to_si;
  rebuilt_remembered_segment *rs;
  uptr address, card;

  if (!rebuilt_remembered.active || !address_is_meaningful
      || FIXMEDIATE(p)
      || (to_si = MaybeSegInfo(ptr_get_segment(p))) == NULL)
    return;

  address = (uptr)TO_PTR(pp);
  from_si = MaybeSegInfo(addr_get_segment(address));
  if (from_si == NULL || !remembered_spacep(from_si->space)
      || from_si->generation == 0
      || to_si->generation >= from_si->generation)
    return;

  rs = remembered_find_segment(from_si->number, 0);
  if (rs == NULL)
    S_error_abort("check_heap: uninventoried remembered-set source");
  card = (address >> card_offset_bits)
       & ((1 << segment_card_offset_bits) - 1);
  remembered_lower(&rs->required[card], to_si->generation);

  if (remembered_object_card_spacep(from_si->space) && base != (ptr)0) {
    uptr start = (uptr)UNTYPE(base, TYPEBITS(base));
    uptr end = start + size_object(base) - 1;
    uptr first_card = start >> card_offset_bits;
    uptr last_card = end >> card_offset_bits;
    uptr object_card;

    for (object_card = first_card; object_card <= last_card; object_card += 1) {
      uptr segment = object_card >> segment_card_offset_bits;
      rebuilt_remembered_segment *object_rs =
        remembered_find_segment(segment, 0);
      if (object_rs == NULL)
        S_error_abort("check_heap: object crosses an uninventoried segment");
      remembered_lower(
        &object_rs->scanned[
          object_card & ((1 << segment_card_offset_bits) - 1)],
        to_si->generation);
    }
  } else {
    remembered_lower(&rs->scanned[card], to_si->generation);
  }
}

static void remembered_verify(void) {
  IGEN from_generation;
  size_t i;

  if (!rebuilt_remembered.active) return;

  if (S_checkheap_test_remembered_source != (ptr)0) {
    ptr source = S_checkheap_test_remembered_source;
    iptr index = S_checkheap_test_remembered_index;
    ptr *location = &INITVECTIT(source, index);
    seginfo *source_si = MaybeSegInfo(ptr_get_segment(source));
    seginfo *target_si = FIXMEDIATE(*location)
                           ? NULL : MaybeSegInfo(ptr_get_segment(*location));
    uptr card = ((uptr)TO_PTR(location) >> card_offset_bits)
              & ((1 << segment_card_offset_bits) - 1);

    S_checkheap_test_remembered_source = (ptr)0;
    S_checkheap_test_remembered_index = 0;
    if (source_si == NULL
        || target_si == NULL
        || source_si->generation == 0
        || target_si->generation >= source_si->generation)
      S_error_abort("checkheap remembered-set fault target is not old-to-young");
    source_si->dirty_bytes[card] = 0xff;
  }

  for (from_generation = 1;
       from_generation <= static_generation;
       from_generation = from_generation == S_G.max_nonstatic_generation
                           ? static_generation : from_generation + 1) {
    IGEN to_generation;
    IGEN maximum_to_generation =
      from_generation == static_generation
        ? S_G.max_nonstatic_generation : from_generation - 1;

    for (to_generation = 0;
         to_generation <= maximum_to_generation;
         to_generation += 1) {
      seginfo *listed;
      size_t entries = 0;
      for (listed = DirtySegments(from_generation, to_generation);
           listed != NULL;
           listed = listed->dirty_next) {
        entries += 1;
        if (entries > rebuilt_remembered.occupied_segment_count) {
          S_checkheap_errors += 1;
          printf("!!! rebuilt remembered set found a cyclic or oversized"
                 " dirty list from generation %d to %d\n",
                 from_generation, to_generation);
          break;
        }
        if (remembered_spacep(listed->space)) {
          rebuilt_remembered_segment *rs =
            remembered_find_segment(listed->number, 0);
          if (rs == NULL)
            S_error_abort("check_heap: dirty list has uninventoried segment");
          rs->list_count += 1;
          if ((unsigned int)to_generation < rs->listed_minimum)
            rs->listed_minimum = (unsigned int)to_generation;
        }
      }
    }
  }

  for (i = 0; i < rebuilt_remembered.capacity; i += 1) {
    rebuilt_remembered_segment *rs = &rebuilt_remembered.segments[i];
    seginfo *si;
    uptr card;
    unsigned int required_min = 0xff;

    if (rs->segment == REMEMBERED_EMPTY_SEGMENT) continue;
    si = SegInfo(rs->segment);
    if (si->generation != rs->generation || si->space != rs->space)
      S_error_abort("check_heap: remembered-set segment changed during rebuild");

    if ((si->min_dirty_byte == 0xff && rs->list_count != 0)
        || (si->min_dirty_byte != 0xff
            && (rs->list_count != 1
                || rs->listed_minimum != si->min_dirty_byte))) {
      S_checkheap_errors += 1;
      printf("!!! rebuilt remembered set found inconsistent dirty index:"
             " generation %d space %d segment "PHtx
             " minimum %d, listed minimum %u, occurrences %u\n",
             rs->generation, rs->space, (ptrdiff_t)rs->segment,
             si->min_dirty_byte, rs->listed_minimum, rs->list_count);
    }

    for (card = 0; card < cards_per_segment; card += 1) {
      unsigned int actual = si->dirty_bytes[card];
      unsigned int required = rs->required[card];
      unsigned int scanned = rs->scanned[card];

      if (actual > required) {
        S_checkheap_errors += 1;
        fprintf(stderr,
                "!!! rebuilt remembered set found missing card:"
                " generation %d space %d segment "PHtx" card %td"
                " has %u, requires at most %u\n",
                rs->generation, rs->space, (ptrdiff_t)rs->segment,
                (ptrdiff_t)card, actual, required);
      }

      if (required < required_min) required_min = required;

      if (actual != 0xff
          && (rs->generation == 0
              || actual >= (unsigned int)rs->generation)) {
        S_checkheap_errors += 1;
        printf("!!! rebuilt remembered set found invalid card generation:"
               " generation %d space %d segment "PHtx" card %td"
               " has %u\n",
               rs->generation, rs->space, (ptrdiff_t)rs->segment,
               (ptrdiff_t)card, actual);
      }

      if (checkheap_noisy && actual != scanned) {
        printf("... rebuilt remembered card is conservative:"
               " generation %d space %d segment "PHtx" card %td"
               " has %u, physical requirement %u, current scan %u\n",
               rs->generation, rs->space, (ptrdiff_t)rs->segment,
               (ptrdiff_t)card, actual, required, scanned);
      }
    }

    if (required_min != 0xff) {
      if (si->min_dirty_byte > required_min) {
        S_checkheap_errors += 1;
        printf("!!! rebuilt remembered set found invalid segment minimum:"
               " generation %d space %d segment "PHtx
               " has %d, requires at most %u\n",
               rs->generation, rs->space, (ptrdiff_t)rs->segment,
               si->min_dirty_byte, required_min);
      }
      if (rs->list_count != 1 || rs->listed_minimum > required_min) {
        S_checkheap_errors += 1;
        printf("!!! rebuilt remembered set found unindexed segment:"
               " generation %d space %d segment "PHtx
               " requires at most %u\n",
               rs->generation, rs->space, (ptrdiff_t)rs->segment,
               required_min);
      }
    }
  }
  remembered_reset();
}

static void check_pointer(ptr *pp, IBOOL address_is_meaningful, IBOOL is_reference, ptr base, uptr seg, ISPC s, IBOOL aftergc) {
  ptr p = *pp;

  if (is_reference)
    p = S_maybe_reference_to_object(p);

  shadow_seed_checked_pointer(pp, p, seg, s);
  remembered_record_pointer(pp, p, address_is_meaningful, base);

  if (!FIXMEDIATE(p)) {
    seginfo *psi = MaybeSegInfo(ptr_get_segment(p));
    if (psi != NULL) {
      if ((psi->space == space_empty)
          || psi->old_space
          || (psi->marked_mask && !(psi->marked_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p))
              /* corner case: a continuation in space_count_pure can refer to code via CLOSENTRY
                 where the entry point doesn't have a mark bit: */
              && !((s == space_count_pure) && (psi->space == space_code))
              /* another corner case: a flonum might be inside a marked inexactnum */
              && !(Sflonump(p)
                   && ((psi->space == space_data) || (psi->space == space_new))
                   && maybe_inexactnum_marked(p, psi)))) {
        S_checkheap_errors += 1;
        printf("!!! dangling reference at %s"PHtx" to "PHtx"%s\n",
               (address_is_meaningful ? "" : "insideof "),
               (ptrdiff_t)(address_is_meaningful ? pp : TO_VOIDP(base)),
               (ptrdiff_t)p, (aftergc ? " after gc" : ""));
        printf("from: "); segment_tell(seg);
        printf("to:   "); segment_tell(ptr_get_segment(p));
        {
          ptr l;
          for (l = S_G.locked_objects[psi->generation]; l != Snil; l = Scdr(l))
            if (Scar(l) == p)
              printf(" in locked\n");
          for (l = S_G.unlocked_objects[psi->generation]; l != Snil; l = Scdr(l))
            if (Scar(l) == p)
              printf(" in unlocked\n");
        }
      }

      if (address_is_meaningful) {
        seginfo *ppsi = MaybeSegInfo(addr_get_segment(TO_PTR(pp)));
        if ((ppsi != NULL)
            && (ppsi->generation > psi->generation)
            /* space_data includes stacks, which are always swept */
            && (ppsi->space != space_data)) {
          uptr card = (uptr)TO_PTR(pp) >> card_offset_bits;
          uptr cardno = card & ((1 << segment_card_offset_bits) - 1);
          if (psi->generation < ppsi->dirty_bytes[cardno]) {
            S_checkheap_errors += 1;
            check_heap_dirty_msg("!!! INVALID", pp);
          }
        }
      }
    }
  }
}

static void check_bignum(ptr p) {
  if (!Sbignump(p))
    printf("!!! not a bignum %p\n", TO_VOIDP(p));
}

static void malformed_continuation(ptr p, const char *reason) {
  fprintf(stderr,
          "malformed continuation %p: %s"
          " (stack=%p length="Ptd" clength="Ptd" link=%p ret=%p)\n",
          TO_VOIDP(p), reason, TO_VOIDP(CONTSTACK(p)),
          (ptrdiff_t)CONTLENGTH(p), (ptrdiff_t)CONTCLENGTH(p),
          TO_VOIDP(CONTLINK(p)), TO_VOIDP(CONTRET(p)));
  S_error_abort("check_heap: malformed continuation");
}

typedef struct {
  uptr base;
  uptr limit;
  ptr owner;
  const char *kind;
} checked_stack_range;

static checked_stack_range *checked_stack_ranges = NULL;
static size_t checked_stack_range_count = 0;
static size_t checked_stack_range_capacity = 0;

static IBOOL continuation_objectp(ptr p) {
  ptr code;
  seginfo *si;

  if (!Sprocedurep(p)) return 0;
  code = CLOSCODE(p);
  si = MaybeSegInfo(ptr_get_segment(code));
  return si != NULL
      && si->space == space_code
      && Scodep(code)
      && (CODETYPE(code) & (code_flag_continuation << code_flags_offset));
}

static IBOOL native_fiber_contextp(ptr p) {
  return continuation_objectp(p)
      && S_G.native_fiber_context_code != Sfalse
      && CLOSCODE(p) == S_G.native_fiber_context_code;
}

#define NATIVE_FIBER_CENSUS_EMPTY ((size_t)-1)

enum {
  native_fiber_control_index = 0,
  native_fiber_context_index = 1,
  native_fiber_handler_stack_index = 2,
  native_fiber_entry_index = 3,
  native_fiber_on_return_index = 4,
  native_fiber_starter_index = 5,
  native_fiber_incoming_source_index = 6,
  native_fiber_incoming_payload_index = 7,
  native_fiber_switch_control_index = 8,
  native_fiber_commit_control_index = 9,
  native_fiber_cache_context_index = 10,
  native_fiber_flags_index = 11,
  native_fiber_id_index = 12,
  native_fiber_pinned_next_index = 13
};

typedef struct {
  ptr object;
  IBOOL fiber_seen;
  IBOOL context_seen;
  size_t current_count;
  size_t claimed_count;
  size_t pinned_count;
  size_t context_owner_count;
  ptr current_tc;
  ptr claimed_tc;
  ptr pinned_tc;
} native_fiber_census_entry;

typedef struct {
  native_fiber_census_entry *entries;
  size_t entry_count;
  size_t entry_capacity;
  size_t *slots;
  size_t slot_capacity;
  IBOOL active;
} native_fiber_census_state;

static native_fiber_census_state native_fiber_census;

static size_t native_fiber_census_hash(ptr p, size_t capacity) {
  uptr n = (uptr)TO_PTR(p);
  n ^= n >> 17;
  n *= (uptr)0xed5ad4bbU;
  n ^= n >> 11;
  return (size_t)n & (capacity - 1);
}

static void native_fiber_census_reset(void) {
  free(native_fiber_census.entries);
  free(native_fiber_census.slots);
  memset(&native_fiber_census, 0, sizeof(native_fiber_census));
}

static void native_fiber_census_rehash(size_t capacity) {
  size_t *slots;
  size_t i;

  slots = (size_t *)malloc(capacity * sizeof(size_t));
  if (slots == NULL)
    S_error_abort("check_heap: cannot allocate native-fiber census index");
  for (i = 0; i < capacity; i += 1)
    slots[i] = NATIVE_FIBER_CENSUS_EMPTY;
  for (i = 0; i < native_fiber_census.entry_count; i += 1) {
    size_t at = native_fiber_census_hash(
      native_fiber_census.entries[i].object, capacity);
    while (slots[at] != NATIVE_FIBER_CENSUS_EMPTY)
      at = (at + 1) & (capacity - 1);
    slots[at] = i;
  }
  free(native_fiber_census.slots);
  native_fiber_census.slots = slots;
  native_fiber_census.slot_capacity = capacity;
}

static native_fiber_census_entry *native_fiber_census_find(ptr p,
                                                            IBOOL create) {
  size_t at;

  if (native_fiber_census.slot_capacity == 0) {
    if (!create) return NULL;
    native_fiber_census_rehash(128);
  } else if (create
             && (native_fiber_census.entry_count + 1) * 2
                  >= native_fiber_census.slot_capacity) {
    native_fiber_census_rehash(native_fiber_census.slot_capacity * 2);
  }

  at = native_fiber_census_hash(p, native_fiber_census.slot_capacity);
  while (native_fiber_census.slots[at] != NATIVE_FIBER_CENSUS_EMPTY) {
    size_t index = native_fiber_census.slots[at];
    if (native_fiber_census.entries[index].object == p)
      return &native_fiber_census.entries[index];
    at = (at + 1) & (native_fiber_census.slot_capacity - 1);
  }
  if (!create) return NULL;

  if (native_fiber_census.entry_count
      == native_fiber_census.entry_capacity) {
    size_t capacity = native_fiber_census.entry_capacity == 0
                        ? 64 : native_fiber_census.entry_capacity * 2;
    native_fiber_census_entry *entries =
      (native_fiber_census_entry *)realloc(
        native_fiber_census.entries,
        capacity * sizeof(native_fiber_census_entry));
    if (entries == NULL)
      S_error_abort("check_heap: cannot enlarge native-fiber census");
    native_fiber_census.entries = entries;
    native_fiber_census.entry_capacity = capacity;
  }

  {
    size_t index = native_fiber_census.entry_count++;
    native_fiber_census_entry *entry = &native_fiber_census.entries[index];
    memset(entry, 0, sizeof(*entry));
    entry->object = p;
    native_fiber_census.slots[at] = index;
    return entry;
  }
}

static void native_fiber_census_fail(ptr object, const char *reason) {
  native_fiber_census_entry *entry =
    native_fiber_census_find(object, 0);
  fprintf(stderr, "native-fiber census failure for %p: %s",
          TO_VOIDP(object), reason);
  if (entry != NULL && entry->fiber_seen) {
    ptr control = RECORDINSTIT(object, native_fiber_control_index);
    ptr flags = RECORDINSTIT(object, native_fiber_flags_index);
    fprintf(stderr,
            " (control=%p flags=%p context=%p current=%td claimed=%td pinned=%td)",
            TO_VOIDP(control), TO_VOIDP(flags),
            TO_VOIDP(RECORDINSTIT(object, native_fiber_context_index)),
            (ptrdiff_t)entry->current_count,
            (ptrdiff_t)entry->claimed_count,
            (ptrdiff_t)entry->pinned_count);
  }
  fputc('\n', stderr);
  S_error_abort("check_heap: native-fiber census invariant");
}

static void native_fiber_census_note(ptr p) {
  native_fiber_census_entry *entry;

  if (!native_fiber_census.active || S_G.native_fiber_rtd == Sfalse
      || RECORDINSTTYPE(p) != S_G.native_fiber_rtd)
    return;
  entry = native_fiber_census_find(p, 1);
  entry->fiber_seen = 1;
}

static void native_fiber_census_note_context(ptr p) {
  native_fiber_census_entry *entry;

  if (!native_fiber_census.active || !native_fiber_contextp(p)) return;
  entry = native_fiber_census_find(p, 1);
  entry->context_seen = 1;
}

void S_register_native_fiber_rtd(ptr rtd, ptr context) {
  if (!Srecordp(rtd) || !continuation_objectp(context))
    S_error_abort("native-fiber RTD registration is invalid");
  if (S_G.native_fiber_rtd != Sfalse && S_G.native_fiber_rtd != rtd)
    S_error_abort("native-fiber RTD registration changed");
  if (S_G.native_fiber_context_code != Sfalse
      && S_G.native_fiber_context_code != CLOSCODE(context))
    S_error_abort("native-fiber context registration changed");
  S_G.native_fiber_rtd = rtd;
  S_G.native_fiber_context_code = CLOSCODE(context);
}

static native_fiber_census_entry *native_fiber_census_require(ptr p,
                                                               const char *root) {
  native_fiber_census_entry *entry;

  if (p == Sfalse || p == Snil || FIXMEDIATE(p))
    native_fiber_census_fail(p, root);
  entry = native_fiber_census_find(p, 0);
  if (entry == NULL || !entry->fiber_seen)
    native_fiber_census_fail(p, root);
  return entry;
}

static iptr native_fiber_census_worker_owner(ptr tc) {
  ptr threadno = THREADNO(tc);
  if (!Sfixnump(threadno) || UNFIX(threadno) < 0)
    native_fiber_census_fail(CURRENTNATIVEFIBER(tc),
                             "worker has an invalid native-thread number");
  return UNFIX(threadno) + 1;
}

static size_t native_fiber_census_owner_count(iptr owner) {
  ptr ls;
  size_t count = 0;

  for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
    ptr tc = (ptr)THREADTC(Scar(ls));
    if (native_fiber_census_worker_owner(tc) == owner) count += 1;
  }
  return count;
}

static void native_fiber_census_validate_record(
  native_fiber_census_entry *entry) {
  ptr fiber = entry->object;
  ptr control = (ptr)ATOMIC_LOAD_IPTR_ACQUIRE(
    (iptr *)&RECORDINSTIT(fiber, native_fiber_control_index));
  ptr flags = RECORDINSTIT(fiber, native_fiber_flags_index);
  ptr context = RECORDINSTIT(fiber, native_fiber_context_index);
  ptr handler_stack = RECORDINSTIT(fiber, native_fiber_handler_stack_index);
  ptr initial_entry = RECORDINSTIT(fiber, native_fiber_entry_index);
  ptr on_return = RECORDINSTIT(fiber, native_fiber_on_return_index);
  ptr starter = RECORDINSTIT(fiber, native_fiber_starter_index);
  ptr pinned_next = RECORDINSTIT(fiber, native_fiber_pinned_next_index);
  iptr bits, state, owner, flag_bits;
  IBOOL schedulerp, pinnedp, migratablep;
  IBOOL owns_context = 0;

  if (!Sfixnump(control) || UNFIX(control) < 0 || !Sfixnump(flags)
      || UNFIX(flags) < 0
      || (UNFIX(flags) & ~native_fiber_flags_mask) != 0
      || !Sfixnump(RECORDINSTIT(fiber, native_fiber_id_index)))
    native_fiber_census_fail(fiber, "record metadata is malformed");

  bits = UNFIX(control);
  state = bits & native_fiber_state_mask;
  owner = bits >> native_fiber_state_bits;
  flag_bits = UNFIX(flags);
  schedulerp = (flag_bits & native_fiber_flag_scheduler) != 0;
  pinnedp = (flag_bits & native_fiber_flag_pinned) != 0;
  migratablep = (flag_bits & native_fiber_flag_migratable) != 0;

  if ((schedulerp && (!pinnedp || migratablep))
      || (!schedulerp && (pinnedp == migratablep)))
    native_fiber_census_fail(fiber, "flags do not select one ownership model");
  if (schedulerp && state == native_fiber_state_new)
    native_fiber_census_fail(fiber, "scheduler fiber has an unadopted lifecycle state");
  if (state == native_fiber_state_parking
      || state == native_fiber_state_finishing
      || state > native_fiber_state_finished)
    native_fiber_census_fail(fiber, "transient or reserved state reached a GC root snapshot");

  if ((state == native_fiber_state_new && owner != 0)
      || (state == native_fiber_state_claimed && owner == 0)
      || (state == native_fiber_state_running && owner == 0)
      || (state == native_fiber_state_parked
          && ((pinnedp && owner == 0) || (migratablep && owner != 0)))
      || (state == native_fiber_state_finished && owner != 0))
    native_fiber_census_fail(fiber, "lifecycle state disagrees with owner");
  if (owner != 0 && native_fiber_census_owner_count(owner) != 1)
    native_fiber_census_fail(fiber, "owner does not identify one live worker");

  if (RECORDINSTIT(fiber, native_fiber_incoming_source_index) != Sfalse
      || RECORDINSTIT(fiber, native_fiber_incoming_payload_index) != Sfalse
      || RECORDINSTIT(fiber, native_fiber_switch_control_index) != Sfalse
      || RECORDINSTIT(fiber, native_fiber_commit_control_index) != Sfalse
      || RECORDINSTIT(fiber, native_fiber_cache_context_index) != Sfalse)
    native_fiber_census_fail(fiber, "stable record retains transition scratch");

  switch (state) {
    case native_fiber_state_new:
      owns_context = 1;
      if (handler_stack != Sfalse || !Sprocedurep(initial_entry)
          || !Sprocedurep(on_return) || !Sprocedurep(starter))
        native_fiber_census_fail(fiber, "new fiber has invalid entry roots");
      break;
    case native_fiber_state_claimed:
      owns_context = 1;
      if (!((Sprocedurep(initial_entry) && Sprocedurep(on_return)
             && Sprocedurep(starter) && handler_stack == Sfalse)
            || (initial_entry == Sfalse && on_return == Sfalse
                && starter == Sfalse)))
        native_fiber_census_fail(fiber, "claimed fiber has mixed entry roots");
      break;
    case native_fiber_state_running:
      if (context != Sfalse || handler_stack != Sfalse || starter != Sfalse
          || !((Sprocedurep(initial_entry) && Sprocedurep(on_return))
               || (initial_entry == Sfalse && on_return == Sfalse)))
        native_fiber_census_fail(fiber, "running fiber retains parked roots");
      break;
    case native_fiber_state_parked:
      owns_context = 1;
      if (initial_entry != Sfalse || on_return != Sfalse || starter != Sfalse)
        native_fiber_census_fail(fiber, "parked fiber retains entry roots");
      break;
    case native_fiber_state_finished:
      if (context != Sfalse || handler_stack != Sfalse
          || initial_entry != Sfalse || on_return != Sfalse
          || starter != Sfalse)
        native_fiber_census_fail(fiber, "finished fiber retains execution roots");
      break;
    default:
      native_fiber_census_fail(fiber, "unknown lifecycle state");
  }

  if (owns_context) {
    native_fiber_census_entry *context_entry;
    if (context == Sfalse || !native_fiber_contextp(context))
      native_fiber_census_fail(fiber, "fiber does not own a private stack descriptor");
    context_entry = native_fiber_census_find(context, 0);
    if (context_entry == NULL || !context_entry->context_seen)
      native_fiber_census_fail(fiber, "stack descriptor is absent from strong traversal");
    context_entry->context_owner_count += 1;
  }

  if (schedulerp || state == native_fiber_state_new
      || state == native_fiber_state_finished || migratablep) {
    if (pinned_next != Sfalse)
      native_fiber_census_fail(fiber, "fiber has an unexpected pinned-list link");
  }
}

static void native_fiber_census_validate_workers(void) {
  ptr ls;

  for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
    ptr tc = (ptr)THREADTC(Scar(ls));
    iptr worker_owner = native_fiber_census_worker_owner(tc);
    ptr current = CURRENTNATIVEFIBER(tc);
    ptr claimed = NATIVEFIBERCLAIMED(tc);
    ptr claimed_control = NATIVEFIBERCLAIMEDCONTROL(tc);
    ptr node;

    if (!Sfixnump(FIBERSWITCHPROHIBITEDDEPTH(tc))
        || UNFIX(FIBERSWITCHPROHIBITEDDEPTH(tc)) < 0)
      native_fiber_census_fail(current,
        "worker has an invalid switch-prohibition depth");
    if (NATIVEFIBERTRANSITION(tc) != Sfalse)
      native_fiber_census_fail(current, "worker stopped inside a fiber transition");
    if (NATIVEFIBERPREEMPTACTIVE(tc) != Sfalse
        && NATIVEFIBERPREEMPTACTIVE(tc) != Strue)
      native_fiber_census_fail(current,
        "worker has an invalid preemption-window state");
    if (NATIVEFIBERTESTACTIVE(tc) != Sfalse)
      native_fiber_census_fail(current, "worker stopped inside a transition test hook");

    if (current != Sfalse) {
      native_fiber_census_entry *entry =
        native_fiber_census_require(current,
          "current root is not a strongly reachable native fiber");
      ptr control = RECORDINSTIT(current, native_fiber_control_index);
      if (!Sfixnump(control)
          || (UNFIX(control) & native_fiber_state_mask)
               != native_fiber_state_running
          || (UNFIX(control) >> native_fiber_state_bits) != worker_owner)
        native_fiber_census_fail(current, "current root disagrees with worker ownership");
      entry->current_count += 1;
      entry->current_tc = tc;
    }

    if (claimed == Sfalse) {
      if (claimed_control != Sfalse)
        native_fiber_census_fail(current, "worker retains a claim rollback without a target");
    } else {
      native_fiber_census_entry *entry =
        native_fiber_census_require(claimed,
          "claim root is not a strongly reachable native fiber");
      ptr control = RECORDINSTIT(claimed, native_fiber_control_index);
      ptr flags = RECORDINSTIT(claimed, native_fiber_flags_index);
      iptr old_state, old_owner;
      if (claimed == current || !Sfixnump(control)
          || !Sfixnump(claimed_control)
          || (UNFIX(control) & native_fiber_state_mask)
               != native_fiber_state_claimed
          || (UNFIX(control) >> native_fiber_state_bits) != worker_owner)
        native_fiber_census_fail(claimed, "claim root disagrees with worker ownership");
      old_state = UNFIX(claimed_control) & native_fiber_state_mask;
      old_owner = UNFIX(claimed_control) >> native_fiber_state_bits;
      if (old_state != native_fiber_state_new
          && old_state != native_fiber_state_parked)
        native_fiber_census_fail(claimed, "claim rollback state is not claimable");
      if ((old_state == native_fiber_state_new && old_owner != 0)
          || (old_state == native_fiber_state_parked
              && (((UNFIX(flags) & native_fiber_flag_pinned) != 0
                     && old_owner != worker_owner)
                  || ((UNFIX(flags) & native_fiber_flag_migratable) != 0
                      && old_owner != 0))))
        native_fiber_census_fail(claimed,
          "claim rollback control disagrees with prior ownership");
      if ((old_state == native_fiber_state_new)
          != Sprocedurep(RECORDINSTIT(claimed, native_fiber_entry_index)))
        native_fiber_census_fail(claimed,
          "claim rollback state disagrees with execution roots");
      entry->claimed_count += 1;
      entry->claimed_tc = tc;
    }

    node = NATIVEFIBERPINNEDHEAD(tc);
    while (node != Sfalse && node != Snil) {
      native_fiber_census_entry *entry =
        native_fiber_census_require(node,
          "pinned-list node is not a strongly reachable native fiber");
      ptr control = RECORDINSTIT(node, native_fiber_control_index);
      ptr flags = RECORDINSTIT(node, native_fiber_flags_index);
      iptr state;
      if (entry->pinned_count != 0)
        native_fiber_census_fail(node, "pinned-list node is duplicated or cyclic");
      if (!Sfixnump(control) || !Sfixnump(flags))
        native_fiber_census_fail(node, "pinned-list node has malformed metadata");
      state = UNFIX(control) & native_fiber_state_mask;
      if ((UNFIX(flags) & native_fiber_flag_pinned) == 0
          || (UNFIX(flags) & native_fiber_flag_scheduler) != 0
          || (UNFIX(control) >> native_fiber_state_bits) != worker_owner
          || (state != native_fiber_state_claimed
              && state != native_fiber_state_running
              && state != native_fiber_state_parked))
        native_fiber_census_fail(node, "pinned-list membership disagrees with lifecycle");
      entry->pinned_count = 1;
      entry->pinned_tc = tc;
      node = RECORDINSTIT(node, native_fiber_pinned_next_index);
    }

    if (NATIVEFIBERPREEMPTTARGET(tc) == Sfalse) {
      if (NATIVEFIBERPREEMPTPAYLOAD(tc) != Sfalse)
        native_fiber_census_fail(current, "preemption payload has no target");
    } else {
      ptr target = NATIVEFIBERPREEMPTTARGET(tc);
      ptr target_control;
      ptr target_flags;
      ptr current_flags;
      (void)native_fiber_census_require(target,
        "preemption target is not a strongly reachable native fiber");
      target_control = RECORDINSTIT(target, native_fiber_control_index);
      target_flags = RECORDINSTIT(target, native_fiber_flags_index);
      current_flags = current == Sfalse
                        ? Sfalse
                        : RECORDINSTIT(current, native_fiber_flags_index);
      if (current == Sfalse || !Sfixnump(target_control)
          || target == current || !Sfixnump(target_flags)
          || !Sfixnump(current_flags)
          || (UNFIX(target_flags) & (native_fiber_flag_scheduler
                                     | native_fiber_flag_pinned))
               != (native_fiber_flag_scheduler | native_fiber_flag_pinned)
          || (UNFIX(current_flags) & native_fiber_flag_scheduler) != 0
          || (UNFIX(target_control) & native_fiber_state_mask)
               != native_fiber_state_parked
          || (UNFIX(target_control) >> native_fiber_state_bits) != worker_owner)
        native_fiber_census_fail(target, "preemption target disagrees with worker state");
    }
  }
}

static void native_fiber_census_verify(void) {
  size_t i;

  if (!native_fiber_census.active) return;
  if (S_G.native_fiber_rtd == Sfalse) {
    if (S_G.native_fiber_context_code != Sfalse)
      S_error_abort("check_heap: native-fiber registration is incomplete");
    return;
  }
  if (S_G.native_fiber_context_code == Sfalse)
    S_error_abort("check_heap: native-fiber registration is incomplete");

  for (i = 0; i < native_fiber_census.entry_count; i += 1)
    if (native_fiber_census.entries[i].fiber_seen)
      native_fiber_census_validate_record(&native_fiber_census.entries[i]);
  native_fiber_census_validate_workers();

  for (i = 0; i < native_fiber_census.entry_count; i += 1) {
    native_fiber_census_entry *entry = &native_fiber_census.entries[i];
    if (entry->fiber_seen) {
      ptr control = RECORDINSTIT(entry->object, native_fiber_control_index);
      ptr flags = RECORDINSTIT(entry->object, native_fiber_flags_index);
      iptr state = UNFIX(control) & native_fiber_state_mask;
      IBOOL schedulerp = (UNFIX(flags) & native_fiber_flag_scheduler) != 0;
      IBOOL pinnedp = (UNFIX(flags) & native_fiber_flag_pinned) != 0;

      if ((state == native_fiber_state_running
             ? entry->current_count != 1 : entry->current_count != 0)
          || (state == native_fiber_state_claimed
                ? entry->claimed_count != 1 : entry->claimed_count != 0))
        native_fiber_census_fail(entry->object,
          "lifecycle state does not have exactly one corresponding worker root");
      if (!schedulerp && pinnedp
          && (state == native_fiber_state_claimed
              || state == native_fiber_state_running
              || state == native_fiber_state_parked)) {
        if (entry->pinned_count != 1)
          native_fiber_census_fail(entry->object,
            "owned pinned fiber is absent from its worker registry");
      } else if (entry->pinned_count != 0) {
        native_fiber_census_fail(entry->object,
          "fiber appears unexpectedly in a pinned registry");
      }
    }
    if (entry->context_seen && entry->context_owner_count != 1)
      native_fiber_census_fail(entry->object,
        "private stack descriptor does not have exactly one fiber owner");
  }
}

static void malformed_stack(const char *kind, ptr owner, const char *reason) {
  fprintf(stderr, "malformed %s stack owned by %p: %s\n",
          kind, TO_VOIDP(owner), reason);
  S_error_abort("check_heap: malformed stack");
}

static void check_stack_storage(const char *kind, ptr owner,
                                uptr base, uptr span) {
  uptr seg, limit_seg;
  seginfo *first_si, *si;

  if (base == 0 || (base & (ptr_bytes - 1)) != 0 || base + span < base)
    malformed_stack(kind, owner, "invalid stack bounds");

  first_si = MaybeSegInfo(addr_get_segment((ptr)base));
  if (first_si == NULL
      || (first_si->space != space_new && first_si->space != space_data))
    malformed_stack(kind, owner, "stack base is not in stack storage");

  seg = addr_get_segment((ptr)base);
  limit_seg = addr_get_segment((ptr)(base + (span == 0 ? 0 : span - 1)));
  for (;;) {
    si = MaybeSegInfo(seg);
    if (si == NULL
        || si->space != first_si->space
        || si->generation != first_si->generation)
      malformed_stack(kind, owner, "stack range crosses unrelated storage");
    if (seg == limit_seg) break;
    seg += 1;
  }
}

static void remember_stack_range(const char *kind, ptr owner,
                                 uptr base, uptr span) {
  checked_stack_range *ranges;
  size_t capacity;

  if (span == 0) return;
  if (checked_stack_range_count == checked_stack_range_capacity) {
    capacity = checked_stack_range_capacity == 0
                 ? 64 : checked_stack_range_capacity * 2;
    ranges = (checked_stack_range *)realloc(
      checked_stack_ranges, capacity * sizeof(checked_stack_range));
    if (ranges == NULL)
      S_error_abort("check_heap: cannot allocate stack ownership inventory");
    checked_stack_ranges = ranges;
    checked_stack_range_capacity = capacity;
  }
  checked_stack_ranges[checked_stack_range_count].base = base;
  checked_stack_ranges[checked_stack_range_count].limit = base + span;
  checked_stack_ranges[checked_stack_range_count].owner = owner;
  checked_stack_ranges[checked_stack_range_count].kind = kind;
  checked_stack_range_count += 1;
}

static void check_code_address_range(uptr base, uptr span,
                                     const char *kind, ptr owner) {
  uptr seg, limit_seg;
  seginfo *si;

  if (base == 0 || base + span < base)
    malformed_stack(kind, owner, "invalid return-point header bounds");
  seg = addr_get_segment((ptr)base);
  limit_seg = addr_get_segment((ptr)(base + (span == 0 ? 0 : span - 1)));
  for (;;) {
    si = MaybeSegInfo(seg);
    if (si == NULL || si->space != space_code)
      malformed_stack(kind, owner, "return point is outside code storage");
    if (seg == limit_seg) break;
    seg += 1;
  }
}

static iptr check_return_point(const char *kind, ptr owner, ptr ret,
                               IBOOL terminal) {
  uptr address = (uptr)ret;
  iptr code_delta, frame_size;
  ptr code;
  seginfo *si;

  if (address < size_rp_header)
    malformed_stack(kind, owner, "invalid return point");
  check_code_address_range(address - size_rp_header, size_rp_header,
                           kind, owner);

  code_delta = ENTRYOFFSET(ret)
             + (iptr)(address - (uptr)TO_PTR(ENTRYOFFSETADDR(ret)));
  if (code_delta < 0 || (uptr)code_delta > address)
    malformed_stack(kind, owner, "invalid return-point code offset");
  code = (ptr)(address - (uptr)code_delta);
  si = MaybeSegInfo(ptr_get_segment(code));
  if (si == NULL || si->space != space_code || !Scodep(code))
    malformed_stack(kind, owner, "return point has no code object");
  if (address < (uptr)TO_PTR(&CODEIT(code, 0))
      || address > (uptr)TO_PTR(&CODEIT(code, CODELEN(code))))
    malformed_stack(kind, owner, "return point lies outside its code object");

  frame_size = ENTRYFRAMESIZE(ret);
  if (frame_size < 0 || (!terminal && frame_size == 0)
      || (frame_size & (ptr_bytes - 1)) != 0)
    malformed_stack(kind, owner, "invalid frame size");

  {
    uptr frame_words = (uptr)frame_size >> log2_ptr_bytes;
    uptr root_slots = frame_words == 0 ? 0 : frame_words - 1;
    ptr mask = ENTRYLIVEMASK(ret);

    if (Sfixnump(mask)) {
      iptr bits = UNFIX(mask);
      if (bits < 0
          || (root_slots < ptr_bits
              && ((uptr)bits >> root_slots) != 0))
        malformed_stack(kind, owner, "live mask exceeds frame bounds");
    } else {
      seginfo *mask_si;
      iptr len;
      uptr allowed_bigits;

      if (FIXMEDIATE(mask)
          || (mask_si = MaybeSegInfo(ptr_get_segment(mask))) == NULL
          || (mask_si->space != space_pure
              && mask_si->space != space_impure
              && mask_si->space != space_data)
          || !Sbignump(mask)
          || BIGSIGN(mask))
        malformed_stack(kind, owner, "live mask is not a nonnegative integer");

      len = BIGLEN(mask);
      if (len <= 0 || BIGIT(mask, 0) == 0)
        malformed_stack(kind, owner, "live mask is not canonical");
      allowed_bigits = root_slots / bigit_bits
                     + (root_slots % bigit_bits != 0);
      if ((uptr)len > allowed_bigits
          || ((uptr)len == allowed_bigits
              && root_slots % bigit_bits != 0
              && (BIGIT(mask, 0) >> (root_slots % bigit_bits)) != 0))
        malformed_stack(kind, owner, "live mask exceeds frame bounds");
    }
  }
  return frame_size;
}

static void check_stack_frames(const char *kind, ptr owner,
                               uptr base, uptr fp, ptr ret) {
  while (fp != base) {
    iptr frame_size = check_return_point(kind, owner, ret, 0);
    if (fp < base || (uptr)frame_size > fp - base)
      malformed_stack(kind, owner, "frame extends outside stack bounds");
    fp -= (uptr)frame_size;
    ret = *(ptr *)TO_VOIDP(fp);
  }
  check_return_point(kind, owner, ret, 1);
}

static void check_cached_native_fiber_context(ptr tc) {
  ptr p = CACHEDFRAME(tc);
  seginfo *si;

  if (p == Sfalse) return;
  si = (!FIXMEDIATE(p) ? MaybeSegInfo(ptr_get_segment(p)) : NULL);
  if (si == NULL || si->generation != 0
      || !continuation_objectp(p)
      || CONTLENGTH(p) != scaled_shot_1_shot_flag
      || CONTCLENGTH(p) != scaled_shot_1_shot_flag
      || (native_fiber_contextp(p)
          && (CONTSTACK(p) != Sfalse
              || CONTLINK(p) != Sfalse
              || CONTRET(p) != Sfalse
              || CONTWINDERS(p) != Snil
              || CONTATTACHMENTS(p) != Snil)))
    malformed_stack("cached native-fiber descriptor", p,
                    "descriptor is not poisoned or uniquely cacheable");
}

static void check_worker_stack_geometry(void) {
  ptr ls;

  for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
    ptr tc = (ptr)THREADTC(Scar(ls));
    uptr base = (uptr)SCHEMESTACK(tc);
    uptr size = (uptr)SCHEMESTACKSIZE(tc);
    uptr fp = (uptr)SFP(tc);
    uptr esp = (uptr)ESP(tc);

    if (size < stack_slop || fp < base || fp >= base + size
        || esp != base + size - stack_slop)
      malformed_stack("worker", tc, "invalid base, SFP, ESP, or size");
    check_stack_storage("worker", tc, base, size);
    check_stack_frames("worker", tc, base, fp, FRAME(tc, 0));
    remember_stack_range("worker", tc, base, size);
    check_cached_native_fiber_context(tc);
  }
}

static int compare_stack_ranges(const void *left, const void *right) {
  const checked_stack_range *a = (const checked_stack_range *)left;
  const checked_stack_range *b = (const checked_stack_range *)right;
  return a->base < b->base ? -1 : a->base > b->base ? 1 : 0;
}

static void check_stack_range_ownership(void) {
  size_t i;

  qsort(checked_stack_ranges, checked_stack_range_count,
        sizeof(checked_stack_range), compare_stack_ranges);
  for (i = 1; i < checked_stack_range_count; i += 1) {
    checked_stack_range *previous = &checked_stack_ranges[i - 1];
    checked_stack_range *current = &checked_stack_ranges[i];
    if (current->base < previous->limit) {
      fprintf(stderr,
              "overlapping stack ownership: %s %p [%p,%p) and %s %p [%p,%p)\n",
              previous->kind, TO_VOIDP(previous->owner),
              TO_VOIDP(previous->base), TO_VOIDP(previous->limit),
              current->kind, TO_VOIDP(current->owner),
              TO_VOIDP(current->base), TO_VOIDP(current->limit));
      S_error_abort("check_heap: stack ownership alias");
    }
  }
}

/* Validate the metadata used to walk a continuation stack before heapcheck
 * dereferences its return points. */
static void check_continuation_layout(ptr p) {
  iptr length = CONTLENGTH(p);
  iptr clength = CONTCLENGTH(p);
  uptr base, span;

  if (length == scaled_shot_1_shot_flag) {
    if (clength != scaled_shot_1_shot_flag)
      malformed_continuation(p, "shot marker mismatch");
    return;
  }

  if (clength < 0
      || (clength & (ptr_bytes - 1)) != 0
      || (length != opportunistic_1_shot_flag
          && (length < clength
              || (length & (ptr_bytes - 1)) != 0)))
    malformed_continuation(p, "invalid stack length");

  base = (uptr)CONTSTACK(p);
  span = (uptr)(length == opportunistic_1_shot_flag ? clength : length);
  check_stack_storage("continuation", p, base, span);
  if ((uptr)clength > span)
    malformed_continuation(p, "copied stack exceeds storage");
  check_stack_frames("continuation", p, base, base + (uptr)clength,
                     CONTRET(p));
  remember_stack_range(native_fiber_contextp(p)
                         ? "parked native fiber" : "continuation",
                       p, base, span);
}

/* The reachability check uses storage that is independent of the collector's
 * mark masks and remembered sets. Object layout is shared with the collector,
 * while roots, worklist state, and visited bits are reconstructed here. */
typedef struct {
  uptr segment;
  IGEN generation;
  unsigned char *bits;
  unsigned char *initial_bits;
  uptr *official_bits;
} shadow_segment;

typedef struct {
  ptr object;
  ptr key;
  ptr value;
  IBOOL activated;
} shadow_ephemeron_entry;

typedef struct {
  ptr pair;
  ptr value;
} shadow_weak_entry;

typedef struct {
  ptr entry;
  ptr object;
  ptr representative;
  ptr tconc;
  size_t ftype_counter;
  IBOOL eligible;
  IBOOL ordered;
  IBOOL target_is_static;
  int expected_action;
  int observed_action;
  int probed;
  int probe_scanned;
  int root_added;
} shadow_guardian_entry;

typedef struct {
  void *address;
  iptr remaining;
} shadow_ftype_counter;

typedef struct {
  shadow_segment *segments;
  size_t segment_capacity;
  size_t segment_count;
  ptr *work;
  size_t work_capacity;
  size_t work_count;
  shadow_ephemeron_entry *ephemerons;
  size_t ephemeron_capacity;
  size_t ephemeron_count;
  shadow_weak_entry *weak_entries;
  size_t weak_capacity;
  size_t weak_count;
  shadow_guardian_entry *guardian_entries;
  size_t guardian_capacity;
  size_t guardian_count;
  size_t *guardian_slots;
  size_t guardian_slot_capacity;
  shadow_ftype_counter *ftype_counters;
  size_t ftype_counter_capacity;
  size_t ftype_counter_count;
  IGEN maximum_collected_generation;
  IGEN minimum_target_generation;
  IGEN maximum_target_generation;
  int phase;
} shadow_state;

static shadow_state shadow;

static void checkmark_object(ptr p);

#define SHADOW_EMPTY_SEGMENT ((uptr)-1)
#define SHADOW_PHASE_NONE 0
#define SHADOW_PHASE_PREPARING 1
#define SHADOW_PHASE_COLLECTING 2
#define SHADOW_OFFICIAL_WORDS \
  ((segment_bitmap_bytes + sizeof(uptr) - 1) / sizeof(uptr))

static size_t shadow_segment_hash(uptr segment, size_t capacity) {
  segment ^= segment >> 17;
  segment *= (uptr)0xed5ad4bbU;
  segment ^= segment >> 11;
  return (size_t)segment & (capacity - 1);
}

static void shadow_rehash(size_t capacity) {
  shadow_segment *old_segments = shadow.segments;
  size_t old_capacity = shadow.segment_capacity;
  size_t i;

  shadow.segments = (shadow_segment *)malloc(capacity * sizeof(shadow_segment));
  if (shadow.segments == NULL)
    S_error_abort("check_heap: cannot allocate shadow-mark segment table");
  shadow.segment_capacity = capacity;
  shadow.segment_count = 0;
  for (i = 0; i < capacity; i += 1) {
    shadow.segments[i].segment = SHADOW_EMPTY_SEGMENT;
    shadow.segments[i].generation = 0;
    shadow.segments[i].bits = NULL;
    shadow.segments[i].initial_bits = NULL;
    shadow.segments[i].official_bits = NULL;
  }

  if (old_segments != NULL) {
    for (i = 0; i < old_capacity; i += 1) {
      if (old_segments[i].segment != SHADOW_EMPTY_SEGMENT) {
        size_t at = shadow_segment_hash(old_segments[i].segment, capacity);
        while (shadow.segments[at].segment != SHADOW_EMPTY_SEGMENT)
          at = (at + 1) & (capacity - 1);
        shadow.segments[at] = old_segments[i];
        shadow.segment_count += 1;
      }
    }
    free(old_segments);
  }
}

static shadow_segment *shadow_find_segment(uptr segment, IBOOL create) {
  size_t at;
  seginfo *si;

  if (shadow.segment_capacity == 0) {
    if (!create) return NULL;
    shadow_rehash(64);
  } else if (create
             && (shadow.segment_count + 1) * 2 >= shadow.segment_capacity) {
    shadow_rehash(shadow.segment_capacity * 2);
  }

  at = shadow_segment_hash(segment, shadow.segment_capacity);
  while (shadow.segments[at].segment != SHADOW_EMPTY_SEGMENT) {
    if (shadow.segments[at].segment == segment)
      return &shadow.segments[at];
    at = (at + 1) & (shadow.segment_capacity - 1);
  }

  if (!create) return NULL;
  si = MaybeSegInfo(segment);
  if (si == NULL)
    S_error_abort("check_heap: cannot shadow an unknown segment");
  shadow.segments[at].bits = (unsigned char *)calloc(1, segment_bitmap_bytes);
  if (shadow.segments[at].bits == NULL)
    S_error_abort("check_heap: cannot allocate shadow-mark bitmap");
  shadow.segments[at].segment = segment;
  shadow.segments[at].generation = si->generation;
  shadow.segment_count += 1;
  return &shadow.segments[at];
}

static unsigned char *shadow_segment_bits(uptr segment) {
  return shadow_find_segment(segment, 1)->bits;
}

static void shadow_push(ptr p) {
  ptr *work;
  size_t capacity;

  if (shadow.work_count == shadow.work_capacity) {
    capacity = shadow.work_capacity == 0 ? 1024 : shadow.work_capacity * 2;
    work = (ptr *)realloc(shadow.work, capacity * sizeof(ptr));
    if (work == NULL)
      S_error_abort("check_heap: cannot enlarge shadow-mark worklist");
    shadow.work = work;
    shadow.work_capacity = capacity;
  }
  shadow.work[shadow.work_count++] = p;
}

static void shadow_pointer(ptr p) {
  seginfo *si;
  unsigned char *bits;
  uptr byte;
  unsigned char bit;

  if (FIXMEDIATE(p) || (si = MaybeSegInfo(ptr_get_segment(p))) == NULL)
    return;

  if (si->space == space_empty
      || si->old_space
      || (si->marked_mask
          && !(si->marked_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p))
          && !(Sflonump(p)
               && ((si->space == space_data) || (si->space == space_new))
               && maybe_inexactnum_marked(p, si)))) {
    S_checkheap_errors += 1;
    fprintf(stderr,
            "!!! shadow marking reached an object rejected by the collector at %p\n",
            TO_VOIDP(p));
    segment_tell(ptr_get_segment(p));
    return;
  }

  bits = shadow_segment_bits(ptr_get_segment(p));
  byte = segment_bitmap_byte(p);
  bit = (unsigned char)segment_bitmap_bit(p);
  if (bits[byte] & bit) return;
  bits[byte] |= bit;
  shadow_push(p);
}

static IBOOL shadow_expected_marked(ptr p) {
  shadow_segment *ss;

  if (FIXMEDIATE(p)) return 1;
  ss = shadow_find_segment(ptr_get_segment(p), 0);
  return ss != NULL
      && (ss->bits[segment_bitmap_byte(p)] & segment_bitmap_bit(p)) != 0;
}

static void shadow_expected_pointer(ptr p) {
  seginfo *si;
  unsigned char *bits;
  uptr byte;
  unsigned char bit;

  if (shadow.phase != SHADOW_PHASE_PREPARING
      || FIXMEDIATE(p)
      || (si = MaybeSegInfo(ptr_get_segment(p))) == NULL)
    return;

  if (si->space == space_empty)
    S_error_abort("check_heap: expected traversal reached empty space");

  bits = shadow_segment_bits(ptr_get_segment(p));
  byte = segment_bitmap_byte(p);
  bit = (unsigned char)segment_bitmap_bit(p);
  if (bits[byte] & bit) return;
  bits[byte] |= bit;
  shadow_push(p);
}

static void shadow_ephemeron(ptr p) {
  shadow_ephemeron_entry *entries;
  size_t capacity;

  if (shadow.phase != SHADOW_PHASE_PREPARING) return;
  if (shadow.ephemeron_count == shadow.ephemeron_capacity) {
    capacity = shadow.ephemeron_capacity == 0
                 ? 64 : shadow.ephemeron_capacity * 2;
    entries = (shadow_ephemeron_entry *)realloc(
      shadow.ephemerons, capacity * sizeof(shadow_ephemeron_entry));
    if (entries == NULL)
      S_error_abort("check_heap: cannot enlarge shadow ephemeron inventory");
    shadow.ephemerons = entries;
    shadow.ephemeron_capacity = capacity;
  }
  shadow.ephemerons[shadow.ephemeron_count].object = p;
  shadow.ephemerons[shadow.ephemeron_count].key = Scar(p);
  shadow.ephemerons[shadow.ephemeron_count].value = Scdr(p);
  shadow.ephemerons[shadow.ephemeron_count].activated = 0;
  shadow.ephemeron_count += 1;
}

static void shadow_weak_pair(ptr p) {
  shadow_weak_entry *entries;
  size_t capacity;

  /* An ordered guardian can scan through a representative without retaining
   * the representative itself. Only inventory weak pairs that are themselves
   * in the expected live set. */
  if (shadow.phase != SHADOW_PHASE_PREPARING
      || !shadow_expected_marked(p))
    return;
  if (shadow.weak_count == shadow.weak_capacity) {
    capacity = shadow.weak_capacity == 0 ? 64 : shadow.weak_capacity * 2;
    entries = (shadow_weak_entry *)realloc(
      shadow.weak_entries, capacity * sizeof(shadow_weak_entry));
    if (entries == NULL)
      S_error_abort("check_heap: cannot enlarge shadow weak-pair inventory");
    shadow.weak_entries = entries;
    shadow.weak_capacity = capacity;
  }
  shadow.weak_entries[shadow.weak_count].pair = p;
  shadow.weak_entries[shadow.weak_count].value = Scar(p);
  shadow.weak_count += 1;
}

static IBOOL shadow_ephemeron_key_live(ptr key) {
  shadow_segment *ss;

  if (key == Sbwp_object) return 0;
  if (FIXMEDIATE(key)) return 1;
  ss = shadow_find_segment(ptr_get_segment(key), 0);
  if (ss == NULL || ss->generation > shadow.maximum_collected_generation)
    return 1;
  return shadow_expected_marked(key);
}

static void shadow_drain_expected(void) {
  for (;;) {
    size_t i;
    IBOOL changed = 0;

    while (shadow.work_count != 0)
      checkmark_object(shadow.work[--shadow.work_count]);

    for (i = 0; i < shadow.ephemeron_count; i += 1) {
      shadow_ephemeron_entry *entry = &shadow.ephemerons[i];
      if (!entry->activated && shadow_ephemeron_key_live(entry->key)) {
        entry->activated = 1;
        shadow_expected_pointer(entry->value);
        changed = 1;
      }
    }
    if (!changed && shadow.work_count == 0) break;
  }
}

#define SHADOW_GUARDIAN_EMPTY ((size_t)-1)

static size_t shadow_guardian_hash(ptr entry, size_t capacity) {
  uptr n = (uptr)TO_PTR(entry);
  n ^= n >> 17;
  n *= (uptr)0xed5ad4bbU;
  n ^= n >> 11;
  return (size_t)n & (capacity - 1);
}

static void shadow_guardian_rehash(size_t capacity) {
  size_t *old_slots = shadow.guardian_slots;
  size_t old_capacity = shadow.guardian_slot_capacity;
  size_t i;

  shadow.guardian_slots = (size_t *)malloc(capacity * sizeof(size_t));
  if (shadow.guardian_slots == NULL)
    S_error_abort("check_heap: cannot allocate guardian index");
  shadow.guardian_slot_capacity = capacity;
  for (i = 0; i < capacity; i += 1)
    shadow.guardian_slots[i] = SHADOW_GUARDIAN_EMPTY;

  if (old_slots != NULL) {
    for (i = 0; i < old_capacity; i += 1) {
      size_t index = old_slots[i];
      if (index != SHADOW_GUARDIAN_EMPTY) {
        size_t at = shadow_guardian_hash(
          shadow.guardian_entries[index].entry, capacity);
        while (shadow.guardian_slots[at] != SHADOW_GUARDIAN_EMPTY)
          at = (at + 1) & (capacity - 1);
        shadow.guardian_slots[at] = index;
      }
    }
    free(old_slots);
  }
}

static shadow_guardian_entry *shadow_find_guardian(ptr entry) {
  size_t at;

  if (shadow.guardian_slot_capacity == 0) return NULL;
  at = shadow_guardian_hash(entry, shadow.guardian_slot_capacity);
  while (shadow.guardian_slots[at] != SHADOW_GUARDIAN_EMPTY) {
    shadow_guardian_entry *g =
      &shadow.guardian_entries[shadow.guardian_slots[at]];
    if (g->entry == entry) return g;
    at = (at + 1) & (shadow.guardian_slot_capacity - 1);
  }
  return NULL;
}

static IGEN shadow_target_generation(IGEN generation) {
  if (generation == shadow.maximum_target_generation)
    return generation;
  if (generation < shadow.minimum_target_generation)
    return shadow.minimum_target_generation;
  return generation + 1;
}

#define SHADOW_NO_FTYPE_COUNTER ((size_t)-1)

static size_t shadow_add_ftype_counter(void *address) {
  shadow_ftype_counter *counters;
  size_t i, capacity;

  for (i = 0; i < shadow.ftype_counter_count; i += 1)
    if (shadow.ftype_counters[i].address == address)
      return i;

  if (shadow.ftype_counter_count == shadow.ftype_counter_capacity) {
    capacity = shadow.ftype_counter_capacity == 0
                 ? 16 : shadow.ftype_counter_capacity * 2;
    counters = (shadow_ftype_counter *)realloc(
      shadow.ftype_counters, capacity * sizeof(shadow_ftype_counter));
    if (counters == NULL)
      S_error_abort("check_heap: cannot enlarge ftype-counter inventory");
    shadow.ftype_counters = counters;
    shadow.ftype_counter_capacity = capacity;
  }

  i = shadow.ftype_counter_count++;
  shadow.ftype_counters[i].address = address;
  shadow.ftype_counters[i].remaining = *(volatile iptr *)address;
  return i;
}

static void shadow_add_guardian(ptr entry, IBOOL eligible) {
  shadow_guardian_entry *entries, *g;
  seginfo *entry_si;
  size_t capacity, at, index;

  if (shadow_find_guardian(entry) != NULL)
    S_error_abort("check_heap: duplicate guardian inventory entry");

  if (shadow.guardian_slot_capacity == 0)
    shadow_guardian_rehash(64);
  else if ((shadow.guardian_count + 1) * 2
           >= shadow.guardian_slot_capacity)
    shadow_guardian_rehash(shadow.guardian_slot_capacity * 2);

  if (shadow.guardian_count == shadow.guardian_capacity) {
    capacity = shadow.guardian_capacity == 0
                 ? 64 : shadow.guardian_capacity * 2;
    entries = (shadow_guardian_entry *)realloc(
      shadow.guardian_entries, capacity * sizeof(shadow_guardian_entry));
    if (entries == NULL)
      S_error_abort("check_heap: cannot enlarge guardian inventory");
    shadow.guardian_entries = entries;
    shadow.guardian_capacity = capacity;
  }

  index = shadow.guardian_count++;
  g = &shadow.guardian_entries[index];
  memset(g, 0, sizeof(*g));
  g->entry = entry;
  g->object = GUARDIANOBJ(entry);
  g->representative = GUARDIANREP(entry);
  g->tconc = GUARDIANTCONC(entry);
  g->ftype_counter = SHADOW_NO_FTYPE_COUNTER;
  g->eligible = eligible;
  g->ordered = GUARDIANORDERED(entry) == Strue;
  entry_si = SegInfo(ptr_get_segment(entry));
  g->target_is_static =
    shadow_target_generation(entry_si->generation) == static_generation;

  if (g->representative == ftype_guardian_rep && eligible) {
    ptr address = RECORDINSTIT(g->object, 0);
    g->ftype_counter = shadow_add_ftype_counter(TO_VOIDP(address));
  }

  at = shadow_guardian_hash(entry, shadow.guardian_slot_capacity);
  while (shadow.guardian_slots[at] != SHADOW_GUARDIAN_EMPTY)
    at = (at + 1) & (shadow.guardian_slot_capacity - 1);
  shadow.guardian_slots[at] = index;
}

static IBOOL shadow_value_live(ptr p) {
  shadow_segment *ss;

  if (p == Sbwp_object) return 0;
  if (FIXMEDIATE(p)) return 1;
  ss = shadow_find_segment(ptr_get_segment(p), 0);
  if (ss == NULL || ss->generation > shadow.maximum_collected_generation)
    return 1;
  return shadow_expected_marked(p);
}

static void shadow_set_guardian_action(shadow_guardian_entry *g,
                                       int action) {
  if (g->expected_action != 0)
    S_error_abort("check_heap: guardian oracle decided an entry twice");

  if (action == CHECKHEAP_GUARDIAN_HOLD && g->target_is_static)
    action = CHECKHEAP_GUARDIAN_DROP;
  if (action == CHECKHEAP_GUARDIAN_FINAL
      && g->ftype_counter != SHADOW_NO_FTYPE_COUNTER) {
    shadow_ftype_counter *counter =
      &shadow.ftype_counters[g->ftype_counter];
    if (counter->remaining <= 0)
      S_error_abort("check_heap: invalid ftype guardian reference count");
    counter->remaining -= 1;
    if (counter->remaining != 0)
      action = CHECKHEAP_GUARDIAN_DROP;
  }
  g->expected_action = action;
}

static void shadow_apply_guardian_roots(void) {
  size_t i;

  for (i = 0; i < shadow.guardian_count; i += 1) {
    shadow_guardian_entry *g = &shadow.guardian_entries[i];
    if (!g->root_added
        && (g->expected_action == CHECKHEAP_GUARDIAN_HOLD
            || g->expected_action == CHECKHEAP_GUARDIAN_FINAL)) {
      ptr representative =
        g->representative == ftype_guardian_rep
          ? g->object : g->representative;
      g->root_added = 1;
      shadow_expected_pointer(representative);
    }
  }
  shadow_drain_expected();
}

static void shadow_scan_guardian_probes(void) {
  size_t i;

  for (i = 0; i < shadow.guardian_count; i += 1) {
    shadow_guardian_entry *g = &shadow.guardian_entries[i];
    if (g->probed && !g->probe_scanned) {
      ptr representative = g->representative;
      seginfo *si;
      g->probe_scanned = 1;
      if (!FIXMEDIATE(representative)
          && (si = MaybeSegInfo(ptr_get_segment(representative))) != NULL
          && si->generation <= shadow.maximum_collected_generation)
        checkmark_object(representative);
    }
  }
  shadow_drain_expected();
}

static void shadow_snapshot_guardians(void) {
  IGEN generation;
  ptr threads, ls;

  for (threads = S_threads; threads != Snil; threads = Scdr(threads)) {
    ptr tc = (ptr)THREADTC(Scar(threads));
    for (ls = GUARDIANENTRIES(tc); ls != Snil; ls = GUARDIANNEXT(ls)) {
      ptr object = GUARDIANOBJ(ls);
      seginfo *si = FIXMEDIATE(object)
                      ? NULL : MaybeSegInfo(ptr_get_segment(object));
      shadow_add_guardian(
        ls, si != NULL && si->generation != static_generation);
    }
  }

  for (generation = 0;
       generation <= shadow.maximum_collected_generation;
       generation += 1)
    for (ls = S_G.guardians[generation]; ls != Snil;
         ls = GUARDIANNEXT(ls))
      shadow_add_guardian(ls, 1);
}

static void shadow_compute_guardians(void) {
  size_t i;
  IBOOL changed;

  /* Unordered guardians finalize in batches. Ordered guardians first expose
   * the representative's outgoing closure without retaining the
   * representative itself. */
  do {
    changed = 0;
    for (i = shadow.guardian_count; i != 0; i -= 1) {
      shadow_guardian_entry *g = &shadow.guardian_entries[i - 1];
      if (g->expected_action != 0 || g->probed) continue;
      if (!g->eligible) {
        shadow_set_guardian_action(g, CHECKHEAP_GUARDIAN_DROP);
        changed = 1;
      } else if (shadow_value_live(g->object)) {
        if (g->target_is_static || shadow_value_live(g->tconc)) {
          shadow_set_guardian_action(g, CHECKHEAP_GUARDIAN_HOLD);
          changed = 1;
        }
      } else if (shadow_value_live(g->tconc)) {
        if (g->ordered)
          g->probed = 1;
        else
          shadow_set_guardian_action(g, CHECKHEAP_GUARDIAN_FINAL);
        changed = 1;
      }
    }

    shadow_apply_guardian_roots();
    shadow_scan_guardian_probes();

    for (i = shadow.guardian_count; i != 0; i -= 1) {
      shadow_guardian_entry *g = &shadow.guardian_entries[i - 1];
      if (g->expected_action == 0 && g->probed
          && shadow_value_live(g->object)) {
        shadow_set_guardian_action(g, CHECKHEAP_GUARDIAN_HOLD);
        changed = 1;
      }
    }
    shadow_apply_guardian_roots();
  } while (changed);

  /* Once probing reaches a fixed point, the remaining ordered candidates are
   * finalized. Their representatives can make additional pending guardians
   * actionable, but ordered probing is over for this collection. */
  for (i = shadow.guardian_count; i != 0; i -= 1) {
    shadow_guardian_entry *g = &shadow.guardian_entries[i - 1];
    if (g->expected_action == 0 && g->probed)
      shadow_set_guardian_action(g, CHECKHEAP_GUARDIAN_FINAL);
  }
  shadow_apply_guardian_roots();

  do {
    changed = 0;
    for (i = shadow.guardian_count; i != 0; i -= 1) {
      shadow_guardian_entry *g = &shadow.guardian_entries[i - 1];
      if (g->expected_action != 0) continue;
      if (shadow_value_live(g->object)) {
        if (g->target_is_static || shadow_value_live(g->tconc)) {
          shadow_set_guardian_action(g, CHECKHEAP_GUARDIAN_HOLD);
          changed = 1;
        }
      } else if (shadow_value_live(g->tconc)) {
        shadow_set_guardian_action(g, CHECKHEAP_GUARDIAN_FINAL);
        changed = 1;
      }
    }
    shadow_apply_guardian_roots();
  } while (changed);

  for (i = 0; i < shadow.guardian_count; i += 1)
    if (shadow.guardian_entries[i].expected_action == 0)
      shadow_set_guardian_action(&shadow.guardian_entries[i],
                                 CHECKHEAP_GUARDIAN_DROP);
}

static void shadow_reset(void) {
  size_t i;

  for (i = 0; i < shadow.segment_capacity; i += 1)
    if (shadow.segments[i].segment != SHADOW_EMPTY_SEGMENT)
      free(shadow.segments[i].bits);
  for (i = 0; i < shadow.segment_capacity; i += 1)
    if (shadow.segments[i].segment != SHADOW_EMPTY_SEGMENT)
      free(shadow.segments[i].initial_bits);
  for (i = 0; i < shadow.segment_capacity; i += 1)
    if (shadow.segments[i].segment != SHADOW_EMPTY_SEGMENT)
      free(shadow.segments[i].official_bits);
  free(shadow.segments);
  free(shadow.work);
  free(shadow.ephemerons);
  free(shadow.weak_entries);
  free(shadow.guardian_entries);
  free(shadow.guardian_slots);
  free(shadow.ftype_counters);
  memset(&shadow, 0, sizeof(shadow));
  native_fiber_census_reset();
}

#include "heapcheck.inc"

static void shadow_seed_checked_pointer(ptr *pp, ptr p, uptr seg, ISPC s) {
  seginfo *si;

  if (shadow.phase != SHADOW_PHASE_PREPARING
      || (si = MaybeSegInfo(seg)) == NULL
      || si->generation <= shadow.maximum_collected_generation)
    return;

  if (s == space_weakpair) {
    uptr word = ((uptr)TO_PTR(pp) - (uptr)build_ptr(seg, 0)) / sizeof(ptr);
    if ((word & 1) == 0)
      shadow_expected_pointer(TYPE(TO_PTR(pp), type_pair));
    else
      shadow_expected_pointer(p);
  } else if (s == space_ephemeron) {
    uptr offset = ((uptr)TO_PTR(pp) - (uptr)build_ptr(seg, 0))
                % size_ephemeron;
    if (offset == 0)
      shadow_expected_pointer(TYPE(TO_PTR(pp), type_pair));
  } else {
    shadow_expected_pointer(p);
  }
}

void S_checkheap_begin_mark_check(IGEN mcg, IGEN min_tg, IGEN max_tg,
                                  ptr count_roots_ls) {
  IGEN g;
  ISPC s;
  uptr i;
  ptr ls;

  shadow_reset();
  shadow.phase = SHADOW_PHASE_PREPARING;
  native_fiber_census.active = 1;
  shadow.maximum_collected_generation = mcg;
  shadow.minimum_target_generation = min_tg;
  shadow.maximum_target_generation = max_tg;

  if (S_checkheap_test_mark_action != 0) {
    seginfo *si = MaybeSegInfo(
      ptr_get_segment(S_checkheap_test_mark_target));
    if (si == NULL || si->generation > mcg)
      S_error_abort("checkheap mark fault target is not collected");
  }

  for (g = 0; g <= mcg; INCRGEN(g))
    for (s = 0; s <= max_real_space; s += 1) {
      seginfo *si;
      for (si = S_G.occupied_segments[g][s]; si != NULL; si = si->next) {
        shadow_segment *ss = shadow_find_segment(si->number, 1);
        ss->official_bits = (uptr *)calloc(SHADOW_OFFICIAL_WORDS,
                                           sizeof(uptr));
        if (ss->official_bits == NULL)
          S_error_abort("check_heap: cannot allocate official-mark bitmap");
      }
    }

  shadow_expected_pointer(S_threads);
  for (i = 0; i < S_G.protect_next; i += 1)
    shadow_expected_pointer(*S_G.protected[i]);
  for (g = 0; g <= static_generation; INCRGEN(g))
    for (ls = S_G.locked_objects[g]; ls != Snil; ls = Scdr(ls))
      shadow_expected_pointer(Scar(ls));

  if (count_roots_ls != Sfalse)
    for (ls = count_roots_ls; ls != Snil; ls = Scdr(ls)) {
      seginfo *si = MaybeSegInfo(ptr_get_segment(ls));
      if (si == NULL
          || (si->space != space_weakpair && si->space != space_ephemeron))
        shadow_expected_pointer(Scar(ls));
    }

  for (i = 0; i < (uptr)S_G.oblist_length; i += 1) {
    bucket *b;
    for (b = S_G.oblist[i]; b != NULL; b = b->next) {
      ptr sym = b->sym;
      if (SYMVAL(sym) != sunbound
          || SYMPLIST(sym) != Snil
          || SYMSPLIST(sym) != Snil)
        shadow_expected_pointer(sym);
    }
  }
}

void S_checkheap_finish_mark_check(void) {
  size_t i;

  if (shadow.phase != SHADOW_PHASE_PREPARING) return;
  shadow_drain_expected();
  native_fiber_census_verify();
  native_fiber_census.active = 0;

  for (i = 0; i < shadow.segment_capacity; i += 1) {
    shadow_segment *ss = &shadow.segments[i];
    if (ss->segment != SHADOW_EMPTY_SEGMENT
        && ss->generation <= shadow.maximum_collected_generation) {
      ss->initial_bits = (unsigned char *)malloc(segment_bitmap_bytes);
      if (ss->initial_bits == NULL)
        S_error_abort("check_heap: cannot allocate initial-mark bitmap");
      memcpy(ss->initial_bits, ss->bits, segment_bitmap_bytes);
    }
  }

  shadow_snapshot_guardians();
  shadow_compute_guardians();
  shadow.phase = SHADOW_PHASE_COLLECTING;
}

void S_checkheap_note_reached(ptr p) {
  shadow_segment *ss;
  uptr byte, word, shift, mask, old;

  if (shadow.phase != SHADOW_PHASE_COLLECTING) return;
  if (S_checkheap_test_mark_action == CHECKHEAP_TEST_MARK_OMIT
      && p == S_checkheap_test_mark_target)
    return;
  ss = shadow_find_segment(ptr_get_segment(p), 0);
  if (ss == NULL || ss->official_bits == NULL)
    S_error_abort("check_heap: collector reached an uninventoried segment");

  byte = segment_bitmap_byte(p);
  word = byte / sizeof(uptr);
  shift = (byte % sizeof(uptr)) * 8;
  mask = ((uptr)segment_bitmap_bit(p)) << shift;
  do {
    old = ss->official_bits[word];
  } while (!COMPARE_AND_SWAP_PTR(&ss->official_bits[word],
                                  (ptr)old, (ptr)(old | mask)));
}

static void shadow_compare_official(const char *stage,
                                    IBOOL use_initial_bits) {
  size_t i;

  for (i = 0; i < shadow.segment_capacity; i += 1) {
    shadow_segment *ss = &shadow.segments[i];
    unsigned char *expected_bits =
      use_initial_bits ? ss->initial_bits : ss->bits;
    uptr byte;
    IBOOL described_segment = 0;
    if (ss->segment == SHADOW_EMPTY_SEGMENT
        || ss->generation > shadow.maximum_collected_generation
        || expected_bits == NULL)
      continue;
    for (byte = 0; byte < segment_bitmap_bytes; byte += 1) {
      uptr word = byte / sizeof(uptr);
      uptr shift = (byte % sizeof(uptr)) * 8;
      unsigned int official = (unsigned int)
        ((ss->official_bits[word] >> shift) & 0xff);
      unsigned int expected = expected_bits[byte];
      unsigned int missing = expected & ~official;
      unsigned int extra = official & ~expected;
      unsigned int bit_index;
      if ((missing != 0 || extra != 0) && !described_segment) {
        segment_tell(ss->segment);
        described_segment = 1;
      }
      for (bit_index = 0; bit_index < 8; bit_index += 1) {
        unsigned int bit = 1U << bit_index;
        if (missing & bit) {
          S_checkheap_errors += 1;
          fprintf(stderr,
                  "!!! %s checkmark found reachable object omitted by collector at %p\n",
                  stage,
                  TO_VOIDP(build_ptr(ss->segment,
                    ((byte << 3) + bit_index) << log2_ptr_bytes)));
        }
      }
      for (bit_index = 0; bit_index < 8; bit_index += 1) {
        unsigned int bit = 1U << bit_index;
        if (extra & bit) {
          S_checkheap_errors += 1;
          fprintf(stderr,
                  "!!! %s checkmark found unreachable object retained by collector at %p\n",
                  stage,
                  TO_VOIDP(build_ptr(ss->segment,
                    ((byte << 3) + bit_index) << log2_ptr_bytes)));
        }
      }
    }
  }
}

void S_checkheap_verify_mark_check(void) {
  if (shadow.phase == SHADOW_PHASE_COLLECTING
      && S_checkheap_test_mark_action == CHECKHEAP_TEST_MARK_EXTRA) {
    ptr p = S_checkheap_test_mark_target;
    shadow_segment *ss = shadow_find_segment(ptr_get_segment(p), 0);
    uptr byte, word, shift;
    if (ss == NULL || ss->official_bits == NULL)
      S_error_abort("checkheap extra-mark fault target is uninventoried");
    byte = segment_bitmap_byte(p);
    word = byte / sizeof(uptr);
    shift = (byte % sizeof(uptr)) * 8;
    ss->official_bits[word] |= ((uptr)segment_bitmap_bit(p)) << shift;
  }
  S_checkheap_test_mark_target = (ptr)0;
  S_checkheap_test_mark_action = 0;
  if (shadow.phase != SHADOW_PHASE_COLLECTING) return;
  shadow_compare_official("initial", 1);
}

void S_checkheap_note_guardian(ptr entry, int action) {
  shadow_guardian_entry *g;

  if (shadow.phase != SHADOW_PHASE_COLLECTING) return;
  g = shadow_find_guardian(entry);
  if (g == NULL) {
    S_checkheap_errors += 1;
    fprintf(stderr,
            "!!! guardian oracle observed an uninventoried entry at %p\n",
            TO_VOIDP(entry));
    return;
  }
  if (g->observed_action != 0) {
    S_checkheap_errors += 1;
    fprintf(stderr,
            "!!! guardian oracle observed entry %p more than once\n",
            TO_VOIDP(entry));
    return;
  }
  g->observed_action = action;
  if (g->expected_action != action) {
    S_checkheap_errors += 1;
    fprintf(stderr,
            "!!! guardian oracle expected action %d but observed %d for entry %p\n",
            g->expected_action, action, TO_VOIDP(entry));
  }
}

static IBOOL shadow_relocated_container(ptr source, ptr *destination) {
  seginfo *si;
  shadow_segment *ss;

  if (FIXMEDIATE(source)) {
    *destination = source;
    return 1;
  }
  ss = shadow_find_segment(ptr_get_segment(source), 0);
  if (ss == NULL || ss->generation > shadow.maximum_collected_generation) {
    *destination = source;
    return 1;
  }
  si = MaybeSegInfo(ptr_get_segment(source));
  if (si == NULL) return 0;
  if (si->marked_mask != NULL
      && (si->marked_mask[segment_bitmap_byte(source)]
          & segment_bitmap_bit(source))) {
    *destination = source;
    return 1;
  }
  if (FWDMARKER(source) == forward_marker) {
    *destination = FWDADDRESS(source);
    return 1;
  }
  return 0;
}

static void shadow_verify_weak_entries(void) {
  size_t i;

  for (i = 0; i < shadow.weak_count; i += 1) {
    shadow_weak_entry *entry = &shadow.weak_entries[i];
    ptr pair;
    IBOOL expected_live = shadow_value_live(entry->value);
    if (!shadow_relocated_container(entry->pair, &pair)) {
      S_checkheap_errors += 1;
      fprintf(stderr,
              "!!! weak oracle lost reachable weak pair %p\n",
              TO_VOIDP(entry->pair));
    } else if ((expected_live && Scar(pair) == Sbwp_object)
               || (!expected_live && Scar(pair) != Sbwp_object)) {
      S_checkheap_errors += 1;
      fprintf(stderr,
              "!!! weak oracle expected %s value for pair %p"
              " (source value %p, current value %p)\n",
              expected_live ? "a live" : "a cleared",
              TO_VOIDP(entry->pair), TO_VOIDP(entry->value),
              TO_VOIDP(Scar(pair)));
    }
  }
}

static void shadow_verify_ephemerons(void) {
  size_t i;

  for (i = 0; i < shadow.ephemeron_count; i += 1) {
    shadow_ephemeron_entry *entry = &shadow.ephemerons[i];
    ptr ephemeron;
    if (!shadow_expected_marked(entry->object)) continue;
    if (!shadow_relocated_container(entry->object, &ephemeron)) {
      S_checkheap_errors += 1;
      fprintf(stderr,
              "!!! ephemeron oracle lost reachable ephemeron %p\n",
              TO_VOIDP(entry->object));
    } else if (entry->activated) {
      if (Scar(ephemeron) == Sbwp_object
          || Scdr(ephemeron) == Sbwp_object) {
        S_checkheap_errors += 1;
        fprintf(stderr,
                "!!! ephemeron oracle cleared live ephemeron %p\n",
                TO_VOIDP(entry->object));
      }
    } else if (Scar(ephemeron) != Sbwp_object
               || Scdr(ephemeron) != Sbwp_object) {
      S_checkheap_errors += 1;
      fprintf(stderr,
              "!!! ephemeron oracle retained dead ephemeron fields at %p\n",
              TO_VOIDP(entry->object));
    }
  }
}

void S_checkheap_verify_finalization_check(seginfo *oldspacesegments) {
  seginfo *si;
  size_t i;

  if (shadow.phase != SHADOW_PHASE_COLLECTING) return;

  for (si = oldspacesegments; si != NULL; si = si->next) {
    ptr entry;
    for (entry = si->trigger_guardians; entry != 0;
         entry = GUARDIANNEXT(entry))
      S_checkheap_note_guardian(entry, CHECKHEAP_GUARDIAN_DROP);
  }

  for (i = 0; i < shadow.guardian_count; i += 1) {
    shadow_guardian_entry *g = &shadow.guardian_entries[i];
    if (g->observed_action == 0) {
      S_checkheap_errors += 1;
      fprintf(stderr,
              "!!! guardian oracle did not observe expected action %d for entry %p\n",
              g->expected_action, TO_VOIDP(g->entry));
    }
  }

  shadow_compare_official("finalization", 0);
  shadow_verify_weak_entries();
  shadow_verify_ephemerons();
  shadow_reset();
}

static void shadow_check_heap(IGEN mcg) {
  IGEN g;
  uptr i;
  ptr ls;

  if (mcg < S_G.max_nonstatic_generation) return;

  shadow_pointer(S_threads);
  for (i = 0; i < S_G.protect_next; i += 1)
    shadow_pointer(*S_G.protected[i]);

  for (g = 0; g <= static_generation; INCRGEN(g)) {
    shadow_pointer(S_G.locked_objects[g]);
    shadow_pointer(S_G.unlocked_objects[g]);
    shadow_pointer(S_G.rtds_with_counts[g]);
    shadow_pointer(S_G.gcbackreference[g]);
#ifndef WIN32
    shadow_pointer(S_child_processes[g]);
#endif
    for (ls = S_G.guardians[g]; ls != Snil; ls = GUARDIANNEXT(ls)) {
      shadow_pointer(GUARDIANREP(ls));
      shadow_pointer(GUARDIANTCONC(ls));
    }
  }

  for (i = 0; i < (uptr)S_G.oblist_length; i += 1) {
    bucket *b;
    for (b = S_G.oblist[i]; b != NULL; b = b->next)
      shadow_pointer(b->sym);
  }

  while (shadow.work_count != 0)
    shadow_object(shadow.work[--shadow.work_count]);

  /* The collector may retain objects reached from temporary GC roots that no
   * longer exist here. The useful independent check is therefore one-way:
   * every object reachable now must have survived the collector. */
  shadow_reset();
}

#ifdef PTHREADS

static ptr *find_nl(ptr *pp1, ISPC s, IGEN g) {
  seginfo *si;
  thread_gc *tgc;

  si = SegInfo(addr_get_segment(TO_PTR(pp1)));
  tgc = si->creator;
  return TO_VOIDP(tgc->next_loc[g][s]);
}


# define FIND_NL(pp1, pp2, s, g) find_nl(pp1, s, g)

#else

/* no `creator` field, so we search the slow way */
static ptr *find_nl(ptr *pp1, ptr *pp2, ISPC s, IGEN g) {
  ptr *nl, ls;

  for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
    ptr t_tc = (ptr)THREADTC(Scar(ls));
    nl = TO_VOIDP(THREAD_GC(t_tc)->next_loc[g][s]);
    if (pp1 <= nl && nl < pp2)
      return nl;
  }

  return NULL;
}
# define FIND_NL(pp1, pp2, s, g) find_nl(pp1, pp2, s, g)

#endif

static void check_heap_dirty_msg(char *msg, ptr *x) {
    INT d; seginfo *si;

    si = SegInfo(addr_get_segment(TO_PTR(x)));
    d = (INT)(((uptr)TO_PTR(x) >> card_offset_bits) & ((1 << segment_card_offset_bits) - 1));
    printf("%s dirty byte %d found in segment "PHtx", card %d at "PHtx"\n", msg, si->dirty_bytes[d], (ptrdiff_t)(si->number), d, (ptrdiff_t)x);
    printf("from "); segment_tell(addr_get_segment(TO_PTR(x)));
    printf("to   "); segment_tell(ptr_get_segment(*x));
}

void S_check_heap(IBOOL aftergc, IGEN mcg) {
  uptr seg; INT d; ISPC s; IGEN g; IDIRTYBYTE dirty; IBOOL found_eos; IGEN pg;
  ptr p, *pp1, *pp2, *nl;
  iptr i, for_code;
  uptr empty_segments = 0;
  uptr used_segments = 0;
  uptr static_segments = 0;
  uptr nonstatic_segments = 0;

  remembered_begin(aftergc, mcg);
  check_dirty();
  checked_stack_range_count = 0;
  check_worker_stack_geometry();

  {
    ptr ls;
    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr t_tc = (ptr)THREADTC(Scar(ls));
      for (s = 0; s <= max_real_space; s += 1) {
        for (g = 0; g <= static_generation; INCRGEN(g)) {
          thread_gc *tgc = THREAD_GC(t_tc);
          if ((tgc->next_loc[g][s] == (ptr)0) != (tgc->base_loc[g][s] == (ptr)0)) {
            S_checkheap_errors += 1;
            printf("!!! inconsistent thread NEXT %p and BASE %p\n",
                   TO_VOIDP(tgc->next_loc[g][s]), TO_VOIDP(tgc->base_loc[g][s]));
          }
        }
      }
    }
  }

  for (for_code = 0; for_code < 2; for_code++) {
    for (i = PARTIAL_CHUNK_POOLS; i >= -1; i -= 1) {
      chunkinfo *chunk = (i == -1
                          ? (for_code ? S_code_chunks_full : S_chunks_full)
                          : (for_code ? S_code_chunks[i] : S_chunks[i]));
      while (chunk != NULL) {
        seginfo *si = chunk->unused_segs;
        iptr count = 0;
        while(si) {
          count += 1;
          if (si->space != space_empty) {
            S_checkheap_errors += 1;
            printf("!!! unused segment has unexpected space\n");
          }
          si = si->next;
        }
        if ((chunk->segs - count) != chunk->nused_segs) {
          S_checkheap_errors += 1;
          printf("!!! unexpected used segs count "Ptd" with "Ptd" total segs and "Ptd" segs on the unused list\n",
                 (ptrdiff_t)chunk->nused_segs, (ptrdiff_t)chunk->segs, (ptrdiff_t)count);
        }
        used_segments += chunk->nused_segs;
        empty_segments += count;
        chunk = chunk->next;
      }
    }
  }

  for (s = 0; s <= max_real_space; s += 1) {
    seginfo *si;
    for (g = 0; g <= S_G.max_nonstatic_generation; INCRGEN(g)) {
      for (si = S_G.occupied_segments[g][s]; si != NULL; si = si->next) {
        if (si->generation != g) {
          S_checkheap_errors += 1;
          printf("!!! segment in wrong occupied_segments list\n");
        }
        nonstatic_segments += 1;
      }
    }
    for (si = S_G.occupied_segments[static_generation][s]; si != NULL; si = si->next) {
      static_segments += 1;
    }
  }

  if (used_segments != nonstatic_segments + static_segments) {
    S_checkheap_errors += 1;
    printf("!!! found "PHtx" used segments and "PHtx" occupied segments\n",
        (ptrdiff_t)used_segments,
        (ptrdiff_t)(nonstatic_segments + static_segments));
  }

  if (S_G.number_of_nonstatic_segments != nonstatic_segments) {
    S_checkheap_errors += 1;
    printf("!!! S_G.number_of_nonstatic_segments "PHtx" is different from occupied number "PHtx"\n",
        (ptrdiff_t)S_G.number_of_nonstatic_segments,
        (ptrdiff_t)nonstatic_segments);
  }

  if (S_G.number_of_empty_segments != empty_segments) {
    S_checkheap_errors += 1;
    printf("!!! S_G.number_of_empty_segments "PHtx" is different from unused number "PHtx"\n",
        (ptrdiff_t)S_G.number_of_empty_segments,
        (ptrdiff_t)empty_segments);
  }

  for (for_code = 0; for_code < 2; for_code++) {
    for (i = PARTIAL_CHUNK_POOLS; i >= -1; i -= 1) {
      chunkinfo *chunk = (i == -1
                          ? (for_code ? S_code_chunks_full : S_chunks_full)
                          : (for_code ? S_code_chunks[i] : S_chunks[i]));
      while (chunk != NULL) {
        uptr nsegs; seginfo *si;
        for (si = &chunk->sis[0], nsegs = chunk->segs; nsegs != 0; nsegs -= 1, si += 1) {
          seginfo *recorded_si; uptr recorded_seg;
          if ((seg = si->number) != (recorded_seg = (chunk->base + chunk->segs - nsegs))) {
            S_checkheap_errors += 1;
            printf("!!! recorded segment number "PHtx" differs from actual segment number "PHtx"", (ptrdiff_t)seg, (ptrdiff_t)recorded_seg);
          }
          if ((recorded_si = SegInfo(seg)) != si) {
            S_checkheap_errors += 1;
            printf("!!! recorded segment "PHtx" seginfo "PHtx" differs from actual seginfo "PHtx"", (ptrdiff_t)seg, (ptrdiff_t)recorded_si, (ptrdiff_t)si);
          }
          s = si->space;
          g = si->generation;

          if (si->use_marks)
            printf("!!! use_marks set on generation %d segment "PHtx"\n", g, (ptrdiff_t)seg);

          if (s == space_new) {
            if (g != 0 && !si->marked_mask) {
              S_checkheap_errors += 1;
              printf("!!! unexpected generation %d segment "PHtx" in space_new\n", g, (ptrdiff_t)seg);
            }
          } else if (s == space_impure || s == space_symbol || s == space_pure || s == space_weakpair || s == space_ephemeron
                     || s == space_immobile_impure || s == space_count_pure || s == space_count_impure || s == space_closure
                     || s == space_pure_typed_object || s == space_continuation || s == space_port || s == space_code
                     || s == space_impure_record || s == space_impure_typed_object || s == space_reference_array) {
            ptr start;
          
            /* check for dangling references */
            pp1 = TO_VOIDP(build_ptr(seg, 0));
            pp2 = TO_VOIDP(build_ptr(seg + 1, 0));

            nl = FIND_NL(pp1, pp2, s, g);
            if (pp1 <= nl && nl < pp2) pp2 = nl;

            if (s == space_pure_typed_object || s == space_port || s == space_code
                || s == space_impure_record || s == space_impure_typed_object) {
              /* only check this segment for objects that start on it */
              uptr before_seg = seg;

              /* Back up over segments for the same space and generation: */
              while (1) {
                seginfo *before_si = MaybeSegInfo(before_seg-1);
                if (!before_si
                    || (before_si->space != si->space)
                    || (before_si->generation != si->generation)
                    || ((before_si->marked_mask == NULL) != (si->marked_mask == NULL)))
                  break;
                before_seg--;
              }

              /* Move forward to reach `seg` again: */
              start = build_ptr(before_seg, 0);
              while (before_seg != seg) {
                ptr *before_pp2, *before_nl;

                before_pp2 = TO_VOIDP(build_ptr(before_seg + 1, 0));
                if ((ptr *)TO_VOIDP(start) > before_pp2) {
                  /* skipped to a further segment */
                  before_seg++;
                } else {
                  before_nl = FIND_NL(TO_VOIDP(start), before_pp2, s, g);
                  if (((ptr*)TO_VOIDP(start)) <= before_nl && before_nl < before_pp2) {
                    /* this segment ends, so move to next segment */
                    before_seg++;
                    if (s == space_code) {
                      /* in the case of code, it's possible for a whole segment to
                         go unused if a large code object didn't fit; give up, just in case */
                      start = build_ptr(seg+1, 0);
                    } else {
                      start = build_ptr(before_seg, 0);
                    }
                  } else {
                    seginfo *before_si = MaybeSegInfo(before_seg);
                    while (((ptr *)TO_VOIDP(start)) < before_pp2) {
                      if (before_si->marked_mask) {
                        if (before_si->marked_mask[segment_bitmap_byte(start)] & segment_bitmap_bit(start)) {
                          start = (ptr)((uptr)start + size_object(TYPE(start, type_typed_object)));
                        } else {
                          /* skip past unmarked */
                          start = (ptr)((uptr)start + byte_alignment);
                        }
                      } else {
                        if (*(ptr *)TO_VOIDP(start) == forward_marker) {
                          /* this segment ends, so move to next segment */
                          if (s == space_code) {
                            start = build_ptr(seg+1, 0);
                          } else {
                            start = build_ptr(before_seg+1, 0);
                          }
                        } else {
                          start = (ptr)((uptr)start + size_object(TYPE(start, type_typed_object)));
                        }
                      }
                    }
                    before_seg++;
                  }
                }
              }

              if (((ptr *)TO_VOIDP(start)) >= pp2) {
                /* previous object extended past the segment */
              }  else {
                pp1 = TO_VOIDP(start);
                while (pp1 < pp2) {
                  if (si->marked_mask) {
                    if (si->marked_mask[segment_bitmap_byte(TO_PTR(pp1))] & segment_bitmap_bit(TO_PTR(pp1))) {
                      p = TYPE(TO_PTR(pp1), type_typed_object);
                      check_object(p, seg, s, aftergc);
                      pp1 = TO_VOIDP((ptr)((uptr)TO_PTR(pp1) + size_object(p)));
                    } else {
                      /* skip past unmarked */
                      pp1 = TO_VOIDP((ptr)((uptr)TO_PTR(pp1) + byte_alignment));
                    }
                  } else {
                    if (*pp1 == forward_marker)
                      break;
                    else {
                      p = TYPE(TO_PTR(pp1), type_typed_object);
                      check_object(p, seg, s, aftergc);
                      pp1 = TO_VOIDP((ptr)((uptr)TO_PTR(pp1) + size_object(p)));
                    }
                  }
                }
              }
            } else if (s == space_continuation) {
              while (pp1 < pp2) {
                if (*pp1 == forward_marker)
                  break;
                if (!si->marked_mask || (si->marked_mask[segment_bitmap_byte(TO_PTR(pp1))] & segment_bitmap_bit(TO_PTR(pp1)))) {
                  p = TYPE(TO_PTR(pp1), type_closure);
                  check_object(p, seg, s, aftergc);
                }
                pp1 = TO_VOIDP((ptr)((uptr)TO_PTR(pp1) + size_continuation));
              }
            } else {
              while (pp1 < pp2) {
                if (!si->marked_mask || (si->marked_mask[segment_bitmap_byte(TO_PTR(pp1))] & segment_bitmap_bit(TO_PTR(pp1)))) {
                  int a;
                  for (a = 0; (a < ptr_alignment) && (pp1 < pp2); a++) {
#define             in_ephemeron_pair_part(pp1, seg) ((((uptr)TO_PTR(pp1) - (uptr)build_ptr(seg, 0)) % size_ephemeron) < size_pair)
                    if ((s == space_ephemeron) && !in_ephemeron_pair_part(pp1, seg)) {
                      /* skip non-pair part of ephemeron */
                    } else {
                      p = *pp1;
                      if (!si->marked_mask && (p == forward_marker)) {
                        pp1 = pp2; /* break out of outer loop */
                        break;
                      } else {
                        check_pointer(pp1, 1, (s == space_reference_array), (ptr)0, seg, s, aftergc);
                      }
                    }
                    pp1 += 1;
                  }
                } else
                  pp1 += ptr_alignment;
              }
            }

            /* further verify that dirty bits are set appropriately; only handles some spaces
               to make sure that the dirty byte is not unnecessarily approximate, but we have also
               checked dirty bytes already via `check_pointer` */
            if (s == space_impure || s == space_symbol || s == space_weakpair || s == space_ephemeron
                || s == space_immobile_impure || s == space_closure) {
              found_eos = 0;
              pp2 = pp1 = TO_VOIDP(build_ptr(seg, 0));
              for (d = 0; d < cards_per_segment; d += 1) {
                if (found_eos) {
                  if (si->dirty_bytes[d] != 0xff) {
                    S_checkheap_errors += 1;
                    printf("!!! Dirty byte set past end-of-segment for segment "PHtx", card %d\n", (ptrdiff_t)seg, d);
                    segment_tell(seg);
                  }
                  continue;
                }

                pp2 += bytes_per_card / sizeof(ptr);
                if (pp1 <= nl && nl < pp2) {
                  found_eos = 1;
                  pp2 = nl;
                }

#ifdef DEBUG
                printf("pp1 = "PHtx", pp2 = "PHtx", nl = "PHtx"\n", (ptrdiff_t)pp1, (ptrdiff_t)pp2, (ptrdiff_t)nl);
                fflush(stdout);
#endif

                dirty = 0xff;
                while (pp1 < pp2) {
                  if (!si->marked_mask || (si->marked_mask[segment_bitmap_byte(TO_PTR(pp1))] & segment_bitmap_bit(TO_PTR(pp1)))) {
                    int a;
                    for (a = 0; (a < ptr_alignment) && (pp1 < pp2); a++) {
                      if ((s == space_ephemeron) && !in_ephemeron_pair_part(pp1, seg)) {
                        /* skip non-pair part of ephemeron */
                      } else {
                        p = *pp1;
                      
                        if (p == forward_marker) {
                          found_eos = 1;
                          pp1 = pp2;
                          break;
                        } else if (!FIXMEDIATE(p)) {
                          seginfo *psi = MaybeSegInfo(ptr_get_segment(p));
                          if ((psi != NULL) && ((pg = psi->generation) < g)) {
                            if (pg < dirty) dirty = pg;
                            if (si->dirty_bytes[d] > pg) {
                              S_checkheap_errors += 1;
                              check_heap_dirty_msg("!!! INVALID", pp1);
                            } else if (checkheap_noisy)
                              check_heap_dirty_msg("... ", pp1);
                          }
                        }
                      }
                      pp1 += 1;
                    }
                  } else {
                    pp1 += ptr_alignment;
                  }
                }
              
                if (checkheap_noisy && si->dirty_bytes[d] < dirty) {
                  /* sweep_dirty won't sweep, and update dirty byte, for
                     cards with dirty pointers to segments older than the
                     maximum copied generation, so we can get legitimate
                     conservative dirty bytes even after gc */
                  printf("... Conservative dirty byte %x (%x) %sfor segment "PHtx" card %d ",
                         si->dirty_bytes[d], dirty,
                         (aftergc ? "after gc " : ""),
                         (ptrdiff_t)seg, d);
                  segment_tell(seg);
                }
              }
            } else {
              /* at least check that no dirty bytes are set beyond the end of the segment */
              if (pp2 < (ptr *)TO_VOIDP(build_ptr(seg + 1, 0))) {
                uptr card = (uptr)TO_PTR(pp2) >> card_offset_bits;
                int d = (int)(card & ((1 << segment_card_offset_bits) - 1));

                for (d++; d < cards_per_segment; d++) {
                  if (si->dirty_bytes[d] != 0xff) {
                    S_checkheap_errors += 1;
                    printf("!!! Dirty byte set past end-of-segment for segment "PHtx", card %d\n", (ptrdiff_t)seg, d);
                    segment_tell(seg);
                  }
                }
              }
            }
          }
          if (aftergc
              && (s != space_empty)
              && (g == 0
                  || (s != space_new && s != space_impure && s != space_symbol && s != space_port && s != space_weakpair && s != space_ephemeron
                      && s != space_impure_record && s != space_impure_typed_object
                      && s != space_immobile_impure && s != space_count_impure && s != space_closure && s != space_reference_array))) {
            for (d = 0; d < cards_per_segment; d += 1) {
              if (si->dirty_bytes[d] != 0xff) {
                S_checkheap_errors += 1;
                printf("!!! Unnecessary dirty byte %x (%x) after gc for segment "PHtx" card %d ",
                       si->dirty_bytes[d], 0xff, (ptrdiff_t)(si->number), d);
                segment_tell(seg);
              }
            }
          }
        }
        chunk = chunk->next;
      }
    }
  }

  {
    for (g = 0; g <= S_G.max_nonstatic_generation; INCRGEN(g)) {
      ptr l;
      for (l = S_G.locked_objects[g]; l != Snil; l = Scdr(l))
        check_locked_object(Scar(l), 1, g, aftergc, mcg);
      for (l = S_G.unlocked_objects[g]; l != Snil; l = Scdr(l))
        check_locked_object(Scar(l), 0, g, aftergc, mcg);
    }
  }

  check_stack_range_ownership();

  remembered_verify();
  if (aftergc) shadow_check_heap(mcg);

  if (S_checkheap_errors) {
    printf("heap check failed%s\n", (aftergc ? " after gc" : ""));
    abort();
  }
}

static IBOOL dirty_listedp(seginfo *x, IGEN from_g, IGEN to_g) {
  seginfo *si = DirtySegments(from_g, to_g);
  while (si != NULL) {
    if (si == x) return 1;
    si = si->dirty_next;
  }
  return 0;
}

static void check_dirty_space(ISPC s) {
  IGEN from_g, to_g, min_to_g; INT d; seginfo *si;

  for (from_g = 0; from_g <= static_generation; from_g += 1) {
    for (si = S_G.occupied_segments[from_g][s]; si != NULL; si = si->next) {
      min_to_g = 0xff;
      for (d = 0; d < cards_per_segment; d += 1) {
        to_g = si->dirty_bytes[d];
        if (to_g != 0xff) {
          if (to_g < min_to_g) min_to_g = to_g;
          if (from_g == 0) {
            S_checkheap_errors += 1;
            printf("!!! (check_dirty): space %d, generation %d segment "PHtx" card %d is marked dirty\n", s, from_g, (ptrdiff_t)(si->number), d);
          }
        }
      }
      if (min_to_g != si->min_dirty_byte) {
        S_checkheap_errors += 1;
        printf("!!! (check_dirty): space %d, generation %d segment "PHtx" min_dirty_byte is %d while actual min is %d\n",  s, from_g, (ptrdiff_t)(si->number), si->min_dirty_byte, min_to_g);
        segment_tell(si->number);
      } else if (min_to_g != 0xff) {
        if (!dirty_listedp(si, from_g, min_to_g)) {
          S_checkheap_errors += 1;
          printf("!!! (check_dirty): space %d, generation %d segment "PHtx" is marked dirty but not in dirty-segment list\n", s, from_g, (ptrdiff_t)(si->number));
          segment_tell(si->number);
        }
      }
    }
  }
}

static void check_dirty(void) {
  IGEN from_g, to_g; seginfo *si;

  for (from_g = 1; from_g <= static_generation; from_g = from_g == S_G.max_nonstatic_generation ? static_generation : from_g + 1) {
    for (to_g = 0; (from_g == static_generation) ? (to_g <= S_G.max_nonstatic_generation) : (to_g < from_g); to_g += 1) {
      si = DirtySegments(from_g, to_g);
      if (from_g > S_G.max_nonstatic_generation && from_g != static_generation) {
        if (si != NULL) {
          S_checkheap_errors += 1;
          printf("!!! (check_dirty): unexpected nonempty from-generation %d, to-generation %d dirty segment list\n", from_g, to_g);
        }
      } else {
        while (si != NULL) {
          ISPC s = si->space;
          IGEN g = si->generation;
          IGEN mingval = si->min_dirty_byte;
          if (g != from_g) {
            S_checkheap_errors += 1;
            printf("!!! (check_dirty): generation %d segment "PHtx" in %d -> %d dirty list\n", g, (ptrdiff_t)(si->number), from_g, to_g);
          }
          if (mingval != to_g) {
            S_checkheap_errors += 1;
            printf("!!! (check_dirty): dirty byte = %d for segment "PHtx" in %d -> %d dirty list\n", mingval, (ptrdiff_t)(si->number), from_g, to_g);
          }
          if (s != space_new && s != space_impure && s != space_count_impure && s != space_symbol && s != space_port
              && s != space_impure_record && s != space_impure_typed_object && s != space_immobile_impure 
              && s != space_weakpair && s != space_ephemeron && s != space_closure && s != space_reference_array) {
            S_checkheap_errors += 1;
            printf("!!! (check_dirty): unexpected space %d for dirty segment "PHtx"\n", s, (ptrdiff_t)(si->number));
          }
          si = si->dirty_next;
        }
      }
    }
  }

  check_dirty_space(space_impure);
  check_dirty_space(space_symbol);
  check_dirty_space(space_port);
  check_dirty_space(space_impure_record);
  check_dirty_space(space_impure_typed_object);
  check_dirty_space(space_weakpair);
  check_dirty_space(space_ephemeron);
  check_dirty_space(space_immobile_impure);
  check_dirty_space(space_count_impure);
  check_dirty_space(space_closure);
  check_dirty_space(space_reference_array);

  fflush(stdout);
}

static void check_locked_object(ptr p, IBOOL locked, IGEN g, IBOOL aftergc, IGEN mcg)
{
  const char *what = (locked ? "locked" : "unlocked");
  seginfo *psi = MaybeSegInfo(ptr_get_segment(p));
  if (!psi) {
    S_checkheap_errors += 1;
    printf("!!! generation %d %s object has no segment: %p\n", g, what, TO_VOIDP(p));
  } else {
    if (psi->generation != g) {
      S_checkheap_errors += 1;
      printf("!!! generation %d %s object in generation %d segment: %p\n", g, what, psi->generation, TO_VOIDP(p));
    }
    if (!psi->must_mark && locked) {
      S_checkheap_errors += 1;
      printf("!!! generation %d %s object not on must-mark page: %p\n", g, what, TO_VOIDP(p));
    }
    if (!psi->marked_mask) {
      if (aftergc && (psi->generation <= mcg)) {
        S_checkheap_errors += 1;
        printf("!!! %s object not in marked segment: %p\n", what, TO_VOIDP(p));
        printf(" in: "); segment_tell(psi->number);
      }
    } else if (!(psi->marked_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p))) {
      S_checkheap_errors += 1;
      printf("!!! generation %d %s object not marked: %p\n", g, what, TO_VOIDP(p));
    }
  }
}

void S_fixup_counts(ptr counts) {
  IGEN g; U64 timestamp;

  timestamp = RTDCOUNTSTIMESTAMP(counts);
  for (g = 0; g <= static_generation; INCRGEN(g)) {
    if (timestamp >= S_G.gctimestamp[g]) break;
    RTDCOUNTSIT(counts, g) = 0;
  }
  RTDCOUNTSTIMESTAMP(counts) = S_G.gctimestamp[0];
}

ptr S_do_gc(IGEN max_cg, IGEN min_tg, IGEN max_tg, ptr count_roots) {
  ptr tc = get_thread_context();
  ptr code, result;

  code = CP(tc);
  if (Sprocedurep(code)) code = CLOSCODE(code);
  Slock_object(code);

 /* Scheme side grabs mutex before calling S_do_gc */
  THREAD_GC(tc)->during_alloc += 1;

  if (S_G.new_max_nonstatic_generation > S_G.max_nonstatic_generation) {
    S_G.min_free_gen = S_G.new_min_free_gen;
    S_G.max_nonstatic_generation = S_G.new_max_nonstatic_generation;
  }

  if (max_tg == max_cg && max_cg == S_G.new_max_nonstatic_generation && max_cg < S_G.max_nonstatic_generation) {
    IGEN new_g, old_g, from_g, to_g; ISPC s; seginfo *si, *nextsi, *tail;
   /* reducing max_nonstatic_generation */
    new_g = S_G.new_max_nonstatic_generation;
    old_g = S_G.max_nonstatic_generation;
   /* first, collect everything to old_g, ignoring min_tg */
    result = S_gc(tc, old_g, old_g, old_g, count_roots);
   /* now transfer old_g info to new_g, and clear old_g info */
    S_G.bytes_of_generation[new_g] = S_G.bytes_of_generation[old_g]; S_G.bytes_of_generation[old_g] = 0;
    for (s = 0; s <= max_real_space; s += 1) {
      S_G.bytes_of_space[new_g][s] = S_G.bytes_of_space[old_g][s]; S_G.bytes_of_space[old_g][s] = 0;
      S_G.occupied_segments[new_g][s] = S_G.occupied_segments[old_g][s]; S_G.occupied_segments[old_g][s] = NULL;
      for (si = S_G.occupied_segments[new_g][s]; si != NULL; si = si->next) {
        si->generation = new_g;
      }
      {
        ptr ls;
        for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
          ptr t_tc = (ptr)THREADTC(Scar(ls));
          thread_gc *tgc = THREAD_GC(t_tc);
          tgc->base_loc[new_g][s] = tgc->base_loc[old_g][s]; tgc->base_loc[old_g][s] = (ptr)0;
          tgc->next_loc[new_g][s] = tgc->next_loc[old_g][s]; tgc->next_loc[old_g][s] = (ptr)0;
          tgc->bytes_left[new_g][s] = tgc->bytes_left[old_g][s]; tgc->bytes_left[old_g][s] = 0;
          tgc->sweep_loc[new_g][s] = tgc->sweep_loc[old_g][s]; tgc->sweep_loc[old_g][s] = 0;
          tgc->sweep_next[new_g][s] = tgc->sweep_next[old_g][s]; tgc->sweep_next[old_g][s] = NULL;
        }
      }
    }
    S_G.guardians[new_g] = S_G.guardians[old_g]; S_G.guardians[old_g] = Snil;
    S_G.locked_objects[new_g] = S_G.locked_objects[old_g]; S_G.locked_objects[old_g] = Snil;
    S_G.unlocked_objects[new_g] = S_G.unlocked_objects[old_g]; S_G.unlocked_objects[old_g] = Snil;
    S_G.buckets_of_generation[new_g] = S_G.buckets_of_generation[old_g]; S_G.buckets_of_generation[old_g] = NULL;
    if (S_G.enable_object_counts) {
      INT i; ptr ls;
      for (i = 0; i < countof_types; i += 1) {
        S_G.countof[new_g][i] = S_G.countof[old_g][i]; S_G.countof[old_g][i] = 0;
        S_G.bytesof[new_g][i] = S_G.bytesof[old_g][i]; S_G.bytesof[old_g][i] = 0;
      }
      S_G.rtds_with_counts[new_g] = S_G.rtds_with_counts[old_g]; S_G.rtds_with_counts[old_g] = Snil;
      for (ls = S_G.rtds_with_counts[new_g]; ls != Snil; ls = Scdr(ls)) {
        ptr counts = RECORDDESCCOUNTS(Scar(ls));
        RTDCOUNTSIT(counts, new_g) = RTDCOUNTSIT(counts, old_g); RTDCOUNTSIT(counts, old_g) = 0;
      }
      for (ls = S_G.rtds_with_counts[static_generation]; ls != Snil; ls = Scdr(ls)) {
        ptr counts = RECORDDESCCOUNTS(Scar(ls));
        RTDCOUNTSIT(counts, new_g) = RTDCOUNTSIT(counts, old_g); RTDCOUNTSIT(counts, old_g) = 0;
      }
    }
#ifndef WIN32
    S_child_processes[new_g] = S_child_processes[old_g];
#endif

    /* change old_g dirty bytes in static generation to new_g; splice list of old_g
       seginfos onto front of new_g seginfos */
    for (from_g = 1; from_g <= static_generation; INCRGEN(from_g)) {
      for (to_g = 0; (from_g == static_generation) ? (to_g <= S_G.max_nonstatic_generation) : (to_g < from_g); to_g += 1) {
        if ((si = DirtySegments(from_g, to_g)) != NULL) {
          if (from_g == old_g) {
            DirtySegments(from_g, to_g) = NULL;
            DirtySegments(new_g, to_g) = si;
            si->dirty_prev = &DirtySegments(new_g, to_g);
          } else if (from_g == static_generation) {
            if (to_g == old_g) {
              DirtySegments(from_g, to_g) = NULL;
              tail = DirtySegments(from_g, new_g);
              DirtySegments(from_g, new_g) = si;
              si->dirty_prev = &DirtySegments(from_g, new_g);
              for (;;) {
                INT d;
                si->min_dirty_byte = new_g;
                for (d = 0; d < cards_per_segment; d += 1) {
                  if (si->dirty_bytes[d] == old_g) si->dirty_bytes[d] = new_g;
                }
                nextsi = si->dirty_next;
                if (nextsi == NULL) break;
                si = nextsi;
              }
              if (tail != NULL) tail->dirty_prev = &si->dirty_next;
              si->dirty_next = tail;
            } else {
              do {
                INT d;
                for (d = 0; d < cards_per_segment; d += 1) {
                  if (si->dirty_bytes[d] == old_g) si->dirty_bytes[d] = new_g;
                }
                si = si->dirty_next;
              } while (si != NULL);
            }
          } else {
            S_error_abort("S_do_gc(gc): unexpected nonempty dirty segment list");
          }
        }
      }
    }

   /* tell profile_release_counters to scan only through new_g */
    if (S_G.prcgeneration == old_g) S_G.prcgeneration = new_g;

   /* finally reset max_nonstatic_generation */
    S_G.min_free_gen = S_G.new_min_free_gen;
    S_G.max_nonstatic_generation = new_g;
  } else {
    result = S_gc(tc, max_cg, min_tg, max_tg, count_roots);
  }

 /* eagerly give collecting thread, the only one guaranteed to be
    active, a fresh allocation area.  the other threads have to trap
    to get_more_room if and when they awake and try to allocate */
  S_reset_allocation_pointer(tc);

  THREAD_GC(tc)->during_alloc -= 1;

  Sunlock_object(code);

  return result;
}

ptr S_gc(ptr tc, IGEN max_cg, IGEN min_tg, IGEN max_tg, ptr count_roots) {
  if (min_tg == static_generation
      || S_G.enable_object_counts || S_G.enable_object_backreferences
      || (count_roots != Sfalse)) {
    if (S_G.enable_object_backreferences) min_tg = max_tg;
    return S_gc_oce(tc, max_cg, min_tg, max_tg, count_roots);
#if defined(PTHREADS)
  } else if ((S_collect_waiting_threads != 0)
             || (Spairp(S_threads)
                 && Spairp(Scdr(S_threads))
                 && (S_num_preserve_ownership_threads > 0))) {
    return S_gc_par(tc, max_cg, min_tg, max_tg, Sfalse);
#endif
  } else if (max_cg == 0 && min_tg == 1 && max_tg == 1
           && !S_G.must_mark_gen0 && S_G.locked_objects[0] == Snil
           && (S_G.min_mark_gen > 0)) {
    S_gc_011(tc);
    return Svoid;
  } else {
    return S_gc_ocd(tc, max_cg, min_tg, max_tg, Sfalse);
  }
}
