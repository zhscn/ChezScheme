/* foreign1.c
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

#ifdef _WIN32
#  define SCHEME_IMPORT
#  include "scheme.h"
#  undef EXPORT
#  define EXPORT extern __declspec (dllexport)
#else
#include "scheme.h"
#endif
#include <errno.h>
#include <stdlib.h>

#ifndef _WIN32
# include <signal.h>
# include <stdint.h>
#endif

EXPORT int id(int x) {
   return x;
}

EXPORT int idid(int x) {
    return id(id(x));
}

EXPORT int ididid(int x) {
    return idid(id(x));
}

EXPORT unsigned int iduns(unsigned int x) {
   return x;
}

EXPORT iptr idiptr(iptr x) {
   return x;
}

EXPORT iptr idiptr_addr(void) {
   return (iptr)&idiptr;
}

EXPORT double float_id(double x) {
   return x;
}

#define XMKID(prefix,bits,suffix) prefix##bits##suffix
/* build list of results matching description in foreign.stex */
#define XIRT(name, bits, itype, utype) \
  EXPORT ptr name(itype x) { \
    ptr ls = Snil; \
    ls = Scons(Sinteger64((itype)XMKID(Sunsigned,bits,_value)(XMKID(Sunsigned,bits,)((utype)x))), ls); \
    ls = Scons(Sinteger64(XMKID(Sinteger,bits,_value)(XMKID(Sunsigned,bits,)((utype)x))), ls); \
    ls = Scons(Sinteger64((itype)XMKID(Sunsigned,bits,_value)(XMKID(Sinteger,bits,)(x))), ls); \
    ls = Scons(Sinteger64(XMKID(Sinteger,bits,_value)(XMKID(Sinteger,bits,)(x))), ls); \
    return ls; \
  }
/* build list of results matching description in foreign.stex */
#define XURT(name, bits, itype, utype) \
  EXPORT ptr name(itype x) { \
    ptr ls = Snil; \
    ls = Scons(Sunsigned64(XMKID(Sunsigned,bits,_value)(XMKID(Sunsigned,bits,)(x))), ls); \
    ls = Scons(Sunsigned64((utype)XMKID(Sinteger,bits,_value)(XMKID(Sunsigned,bits,)(x))), ls); \
    ls = Scons(Sunsigned64(XMKID(Sunsigned,bits,_value)(XMKID(Sinteger,bits,)((itype)x))), ls); \
    ls = Scons(Sunsigned64((utype)XMKID(Sinteger,bits,_value)(XMKID(Sinteger,bits,)((itype)x))), ls); \
    return ls; \
  }

XIRT(rt_int,,iptr,uptr)
XIRT(rt_int32,32,Sint32_t,Suint32_t)
XIRT(rt_int64,64,Sint64_t,Suint64_t)

XURT(rt_uint,,iptr,uptr)
XURT(rt_uint32,32,Sint32_t,Suint32_t)
XURT(rt_uint64,64,Sint64_t,Suint64_t)
 
#define XTOI(name, bits, type) EXPORT type name(ptr x) { return XMKID(Sinteger,bits,_value)(x); }
#define XTOU(name, bits, type) EXPORT type name(ptr x) { return XMKID(Sunsigned,bits,_value)(x); }

XTOI(to_int,,iptr)
XTOI(to_int32,32,Sint32_t)
XTOI(to_int64,64,Sint64_t)

XTOU(to_uint,,uptr)
XTOU(to_uint32,32,Suint32_t)
XTOU(to_uint64,64,Suint64_t)

#define XID(name,num) name##num
#define XSID(name) S##name

#define XTRY(name, type, rproc)                                         \
  EXPORT ptr XID(name, 2)(ptr p) {                                      \
    type i = 0;                                                         \
    int success = XSID(name)(p, &i, 0);                                 \
    return Scons(Sinteger(success), Scons(rproc(i), Snil));             \
  }                                                                     \
  EXPORT ptr XID(name, 3)(ptr p) {                                      \
    type i = 0;                                                         \
    const char *reason = "untouched";                                   \
    int success = XSID(name)(p, &i, &reason);                           \
    return Scons(Sinteger(success), Scons(rproc(i), Scons(Sstring(reason), Snil))); \
  }

XTRY(try_integer_value, iptr, Sinteger)
XTRY(try_integer32_value, Sint32_t, Sinteger32)
XTRY(try_integer64_value, Sint64_t, Sinteger64)
XTRY(try_unsigned_value, uptr, Sunsigned)
XTRY(try_unsigned32_value, Suint32_t, Sunsigned32)
XTRY(try_unsigned64_value, Suint64_t, Sunsigned64)

#ifdef _WIN32
#include <stdlib.h>
#include <string.h>

EXPORT char *windows_strcpy(char *dst, char *src) {
  return strcpy(dst, src);
}

EXPORT int windows_strcmp(char *dst, char *src) {
  return strcmp(dst, src);
}

EXPORT void *windows_malloc(long n) {
  return malloc(n);
}

EXPORT void windows_free(void *x) {
  free(x);
}
#endif

EXPORT int set_errno_value(int x) {
   errno = x;
   return x + 1;
}

#ifdef _WIN32
#include <windows.h>
EXPORT int set_last_error_value(int x) {
  SetLastError(x);
  return x + 1;
}
#endif

static int in_callback = 0;

EXPORT int call_for_interrupt_test(int (*f)(int), int v) {
  int result;
  in_callback = 1;
  result = f(v);
  in_callback = 0;
  return result;
}

EXPORT int is_in_callback_for_interrupt_test() {
  return in_callback;
}

#ifndef _WIN32
# if defined(__GNUC__) || defined(__clang__)
#  define NO_SANITIZE_ADDRESS __attribute__((no_sanitize_address))
# else
#  define NO_SANITIZE_ADDRESS
# endif

static void *altstack_test_memory;
static size_t altstack_test_size;
static struct sigaction altstack_test_previous_action;
static volatile sig_atomic_t altstack_test_signal;
static volatile sig_atomic_t altstack_test_nested;
static volatile sig_atomic_t altstack_test_depth;
static volatile sig_atomic_t altstack_test_count;
static volatile sig_atomic_t altstack_test_all_on_stack;

static NO_SANITIZE_ADDRESS void altstack_test_forward(
  int sig, siginfo_t *info, void *context)
{
  char marker;
  uintptr_t address = (uintptr_t)&marker;
  uintptr_t low = (uintptr_t)altstack_test_memory;
  uintptr_t high = low + altstack_test_size;

  if (sig != altstack_test_signal || address < low || address >= high)
    altstack_test_all_on_stack = 0;

  altstack_test_depth += 1;
  altstack_test_count += 1;
  if (altstack_test_nested && altstack_test_depth == 1)
    raise(sig);

  if (altstack_test_previous_action.sa_flags & SA_SIGINFO) {
    altstack_test_previous_action.sa_sigaction(sig, info, context);
  } else if (altstack_test_previous_action.sa_handler != SIG_IGN
             && altstack_test_previous_action.sa_handler != SIG_DFL) {
    altstack_test_previous_action.sa_handler(sig);
  }
  altstack_test_depth -= 1;
}

/* Run the signal handler currently installed for `sig` through a temporary
 * alternate signal stack. With `nested` true, the relay re-enters itself once
 * before forwarding both deliveries to the original handler. */
EXPORT int run_signal_altstack_test(int sig, int nested)
{
  stack_t stack, previous_stack;
  struct sigaction action;
  int result = -1;

#ifndef SA_NODEFER
  if (nested) return -5;
#endif

  altstack_test_size = (size_t)SIGSTKSZ * 4;
  altstack_test_memory = malloc(altstack_test_size);
  if (altstack_test_memory == NULL) return -1;

  stack.ss_sp = altstack_test_memory;
  stack.ss_size = altstack_test_size;
  stack.ss_flags = 0;
  if (sigaltstack(&stack, &previous_stack) != 0) goto done;
  if (sigaction(sig, NULL, &altstack_test_previous_action) != 0) {
    result = -2;
    goto restore_stack;
  }

  sigemptyset(&action.sa_mask);
  action.sa_sigaction = altstack_test_forward;
  action.sa_flags = SA_SIGINFO | SA_ONSTACK;
#ifdef SA_NODEFER
  if (nested) action.sa_flags |= SA_NODEFER;
#endif
  altstack_test_signal = sig;
  altstack_test_nested = nested != 0;
  altstack_test_depth = 0;
  altstack_test_count = 0;
  altstack_test_all_on_stack = 1;

  if (sigaction(sig, &action, NULL) != 0) {
    result = -3;
    goto restore_stack;
  }
  if (raise(sig) != 0) {
    result = -4;
  } else if (!altstack_test_all_on_stack) {
    result = 0;
  } else {
    result = (int)altstack_test_count;
  }
  sigaction(sig, &altstack_test_previous_action, NULL);

restore_stack:
  sigaltstack(&previous_stack, NULL);
done:
  free(altstack_test_memory);
  altstack_test_memory = NULL;
  altstack_test_size = 0;
  return result;
}
# undef NO_SANITIZE_ADDRESS
#else
EXPORT int run_signal_altstack_test(int sig, int nested)
{
  (void)sig;
  (void)nested;
  return -5;
}
#endif
