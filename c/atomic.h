/* `STORE_FENCE` is used by the storage-management system.
   `ACQUIRE_FENCE`, `RELEASE_FENCE`, and `COMPARE_AND_SWAP_PTR`
   are also used by runtime publication protocols, including native-fiber
   ownership transfer, and by the pb interpreter.

   It's always ok to map `ACQUIRE_FENCE` and `RELEASE_FENCE` to
   `STORE_FENCE`. For portability, we mainly rely on a
   `__sync_synchronize` intrinsic as provided by reasonably modern
   versions of GCC and Clang. In some cases, more specialized fence
   variants are available via inline assembly or platform-specific
   intrinsics. When inline assembly is written only for `STORE_FENCE`
   below, then the only advantage over using `__sync_synchronize` is
   to support environments with different or very old compilers.

   `COMPARE_AND_SWAP_PTR` supplies atomicity. Callers that require a
   particular memory order use the acquire and release fences explicitly.
   For `COMPARE_AND_SWAP_PTR`, we similarly rely on a GCC/Clang
   `__sync_bool_compare_and_swap` intrinsic. Some inline-assembly
   versions are here --- but, again, the only advantage of those is to
   support environments with different or very old compilers. */

#if !defined(PTHREADS)
# define STORE_FENCE() do { } while (0)
#elif defined(_MSC_VER) && defined(_M_ARM64)
# define STORE_FENCE()   __dmb(_ARM64_BARRIER_ISHST)
# define ACQUIRE_FENCE() __dmb(_ARM64_BARRIER_ISH)
# define RELEASE_FENCE() ACQUIRE_FENCE()
#elif defined(__arm64__) || defined(__aarch64__)
# define STORE_FENCE()   __asm__ __volatile__ ("dmb ishst" : : : "memory")
# define ACQUIRE_FENCE() __asm__ __volatile__ ("dmb ish" : : : "memory")
# define RELEASE_FENCE() ACQUIRE_FENCE()
#elif defined(__arm__)
# if (arm_isa_version >= 7) || (__ARM_ARCH >= 7)
#  define STORE_FENCE()   __asm__ __volatile__ ("dmb ishst" : : : "memory")
#  define ACQUIRE_FENCE() __asm__ __volatile__ ("dmb ish" : : : "memory")
#  define RELEASE_FENCE() ACQUIRE_FENCE()
# else
#  define STORE_FENCE()   __asm__ __volatile__ ("mcr p15, 0, %0, c7, c10, 5" : : "r" (0) : "memory")
#  define ACQUIRE_FENCE() STORE_FENCE()
#  define RELEASE_FENCE() STORE_FENCE()
# endif
#elif defined(__powerpc64__)
# define STORE_FENCE()   __asm__ __volatile__ ("lwsync" : : : "memory")
# define ACQUIRE_FENCE() __asm__ __volatile__ ("sync" : : : "memory")
# define RELEASE_FENCE() ACQUIRE_FENCE()
#elif defined(__powerpc__) || defined(__POWERPC__)
# define STORE_FENCE()   __asm__ __volatile__ ("sync" : : : "memory")
# define ACQUIRE_FENCE() STORE_FENCE()
# define RELEASE_FENCE() STORE_FENCE()
#elif defined(__riscv)
# define STORE_FENCE()   __asm__ __volatile__ ("fence w,rw" : : : "memory")
# define ACQUIRE_FENCE() __asm__ __volatile__ ("fence r,rw" : : : "memory")
# define RELEASE_FENCE() __asm__ __volatile__ ("fence rw,w" : : : "memory")
#elif defined(__loongarch64)
# define STORE_FENCE()   __asm__ __volatile__ ("dbar 0" : : : "memory")
# define ACQUIRE_FENCE() STORE_FENCE()
# define RELEASE_FENCE() STORE_FENCE()
#elif (__GNUC__ >= 5) || C_COMPILER_HAS_BUILTIN(__sync_synchronize)
# define STORE_FENCE() __sync_synchronize()
# define ACQUIRE_FENCE() STORE_FENCE()
# define RELEASE_FENCE() STORE_FENCE()
#else
# define STORE_FENCE() do { } while (0)
#endif

#ifndef ACQUIRE_FENCE
# define ACQUIRE_FENCE() do { } while (0)
#endif
#ifndef RELEASE_FENCE
# define RELEASE_FENCE() do { } while (0)
#endif

/* Some collector metadata and heap words are deliberately observed while an
   owning collector thread updates them. The accesses are word- or byte-sized
   and aligned; these helpers express that protocol to the C memory model
   without imposing stronger ordering than the collector requires. */
#if !defined(PTHREADS)
# define ATOMIC_LOAD_POINTER_ACQUIRE(a) (*(a))
# define ATOMIC_STORE_POINTER_RELEASE(a, v) (*(a) = (v))
# define ATOMIC_LOAD_IPTR_ACQUIRE(a) (*(a))
# define ATOMIC_LOAD_OCTET_RELAXED(a) (*(a))
# define ATOMIC_OR_OCTET_RELAXED(a, v) (*(a) |= (v))
#elif defined(_MSC_VER)
# define ATOMIC_LOAD_POINTER_ACQUIRE(a) \
    _InterlockedCompareExchangePointer((void *volatile *)(a), NULL, NULL)
# define ATOMIC_STORE_POINTER_RELEASE(a, v) \
    ((void)_InterlockedExchangePointer((void *volatile *)(a), (void *)(v)))
# if ptr_bits == 64
#  define ATOMIC_LOAD_IPTR_ACQUIRE(a) \
     ((iptr)_InterlockedCompareExchange64((volatile __int64 *)(a), 0, 0))
# else
#  define ATOMIC_LOAD_IPTR_ACQUIRE(a) \
     ((iptr)_InterlockedCompareExchange((volatile long *)(a), 0, 0))
# endif
# define ATOMIC_LOAD_OCTET_RELAXED(a) \
    ((octet)_InterlockedCompareExchange8((volatile char *)(a), 0, 0))
# define ATOMIC_OR_OCTET_RELAXED(a, v) \
    ((void)_InterlockedOr8((volatile char *)(a), (char)(v)))
#elif (__GNUC__ >= 5) || C_COMPILER_HAS_BUILTIN(__atomic_load_n)
# define ATOMIC_LOAD_POINTER_ACQUIRE(a) __atomic_load_n((a), __ATOMIC_ACQUIRE)
# define ATOMIC_STORE_POINTER_RELEASE(a, v) \
    __atomic_store_n((a), (v), __ATOMIC_RELEASE)
# define ATOMIC_LOAD_IPTR_ACQUIRE(a) __atomic_load_n((a), __ATOMIC_ACQUIRE)
# define ATOMIC_LOAD_OCTET_RELAXED(a) __atomic_load_n((a), __ATOMIC_RELAXED)
# define ATOMIC_OR_OCTET_RELAXED(a, v) \
    ((void)__atomic_fetch_or((a), (v), __ATOMIC_RELAXED))
#elif C_COMPILER_HAS_BUILTIN(__sync_val_compare_and_swap)
# define ATOMIC_LOAD_POINTER_ACQUIRE(a) __sync_val_compare_and_swap((a), 0, 0)
# define ATOMIC_STORE_POINTER_RELEASE(a, v) \
    ((void)__sync_lock_test_and_set((a), (v)))
# define ATOMIC_LOAD_IPTR_ACQUIRE(a) __sync_val_compare_and_swap((a), 0, 0)
# define ATOMIC_LOAD_OCTET_RELAXED(a) __sync_val_compare_and_swap((a), 0, 0)
# define ATOMIC_OR_OCTET_RELAXED(a, v) ((void)__sync_fetch_and_or((a), (v)))
#else
# define ATOMIC_LOAD_POINTER_ACQUIRE(a) (*(void * volatile *)(a))
# define ATOMIC_STORE_POINTER_RELEASE(a, v) (*(void * volatile *)(a) = (v))
# define ATOMIC_LOAD_IPTR_ACQUIRE(a) (*(volatile iptr *)(a))
# define ATOMIC_LOAD_OCTET_RELAXED(a) (*(volatile octet *)(a))
# define ATOMIC_OR_OCTET_RELAXED(a, v) (*(volatile octet *)(a) |= (v))
#endif

#define GC_ATOMIC_CODE_TYPE(code) ATOMIC_LOAD_IPTR_ACQUIRE(&CODETYPE(code))
#define GC_CODE_TYPE(code) CODETYPE(code)

/* Process-wide runtime switches can be observed by mutator and collector
   threads without taking a runtime lock. Keep those accesses visible to the
   C memory model and to thread sanitizers. */
#if !defined(PTHREADS)
# define ATOMIC_LOAD_IBOOL(a) (*(a))
# define ATOMIC_STORE_IBOOL(a, v) (*(a) = (v))
#elif defined(_MSC_VER)
# define ATOMIC_LOAD_IBOOL(a) \
    ((IBOOL)_InterlockedCompareExchange((volatile long *)(a), 0, 0))
# define ATOMIC_STORE_IBOOL(a, v) \
    ((void)_InterlockedExchange((volatile long *)(a), (long)(v)))
#elif (__GNUC__ >= 5) || C_COMPILER_HAS_BUILTIN(__atomic_load_n)
# define ATOMIC_LOAD_IBOOL(a) __atomic_load_n((a), __ATOMIC_ACQUIRE)
# define ATOMIC_STORE_IBOOL(a, v) \
    __atomic_store_n((a), (v), __ATOMIC_RELEASE)
#elif C_COMPILER_HAS_BUILTIN(__sync_val_compare_and_swap)
# define ATOMIC_LOAD_IBOOL(a) __sync_val_compare_and_swap((a), 0, 0)
# define ATOMIC_STORE_IBOOL(a, v) ((void)__sync_lock_test_and_set((a), (v)))
#else
# define ATOMIC_LOAD_IBOOL(a) (*(volatile IBOOL *)(a))
# define ATOMIC_STORE_IBOOL(a, v) (*(volatile IBOOL *)(a) = (v))
#endif
  
#if !defined(PTHREADS)
# define COMPARE_AND_SWAP_PTR(a, old, new) ((*(ptr *)(a) == TO_PTR(old)) ? (*(ptr *)(a) = TO_PTR(new), 1) : 0)
#elif defined(_MSC_VER)
# if ptr_bits == 64
#  define COMPARE_AND_SWAP_PTR(a, old, new) (_InterlockedCompareExchange64((__int64 *)(a), (__int64)(new), (__int64)(old)) == (__int64)(old))
# else
#  define COMPARE_AND_SWAP_PTR(a, old, new) (_InterlockedCompareExchange((long *)(a), (long)(new), (long)(old)) == (long)(old))
# endif
#elif defined(__arm64__) || defined(__aarch64__)
FORCEINLINE int COMPARE_AND_SWAP_PTR(volatile void *addr, void *old_val, void *new_val) {
  I64 ret;
  __asm__ __volatile__ ("mov %0, #0\n\t"
                        "0:\n\t"
                        "ldxr x12, [%1, #0]\n\t"
                        "cmp x12, %2\n\t"
                        "bne 1f\n\t"
                        "stxr w7, %3, [%1, #0]\n\t"
                        "cmp x7, #0\n\t"
                        "bne 1f\n\t"
                        "mov %0, #1\n\t"
                        "1:\n\t"
                        : "=&r" (ret)
                        : "r" (addr), "r" (old_val), "r" (new_val)
                        : "cc", "memory", "x12", "x7");
  return ret;
}
#elif defined(__arm__) && ((arm_isa_version >= 6) || (__ARM_ARCH >= 6))
FORCEINLINE int COMPARE_AND_SWAP_PTR(volatile void *addr, void *old_val, void *new_val) {
  int ret;
  __asm__ __volatile__ ("mov %0, #0\n\t"
                        "0:\n\t"
                        "ldrex r12, [%1]\n\t"
                        "cmp r12, %2\n\t"
                        "bne 1f\n\t"
                        "strex r7, %3, [%1]\n\t"
                        "cmp r7, #0\n\t"
                        "bne 1f\n\t"
                        "it eq\n\t"
                        "moveq %0, #1\n\t"
                        "1:\n\t"
                        : "=&r" (ret)
                        : "r" (addr), "r" (old_val), "r" (new_val)
                        : "cc", "memory", "r12", "r7");
  return ret;
}
#elif (__GNUC__ >= 5) || C_COMPILER_HAS_BUILTIN(__sync_bool_compare_and_swap)
# define COMPARE_AND_SWAP_PTR(a, old, new) __sync_bool_compare_and_swap((ptr *)(a), TO_PTR(old), TO_PTR(new))
#elif defined(__i386__) || defined(__x86_64__)
# if ptr_bits == 64
#   define CAS_OP_SIZE "q"
# else
#   define CAS_OP_SIZE ""
# endif
FORCEINLINE int COMPARE_AND_SWAP_PTR(volatile void *addr, void *old_val, void *new_val) {
  char result;
  __asm__ __volatile__("lock; cmpxchg" CAS_OP_SIZE " %3, %0; setz %1"
                       : "=m"(*(void **)addr), "=q"(result)
                       : "m"(*(void **)addr), "r" (new_val), "a"(old_val)
                       : "memory");
  return (int) result;
}
#elif defined(__powerpc64__)
FORCEINLINE int COMPARE_AND_SWAP_PTR(volatile void *addr, void *old_val, void *new_val) {
  int ret, tmp;
  __asm__ __volatile__ ("li %0, 0\n\t"
                        "0:\n\t"
                        "ldarx   %1,0,%2\n\t"
                        "cmpw    %3,%1\n\t"
                        "bne- 1f\n\t"
                        "stdcx.  %4,0,%2\n\t"
                        "bne- 1f\n\t"
                        "li %0, 1\n\t"
                        "1:\n\t"
                        : "=&r" (ret), "=&r" (tmp)
                        : "r" (addr), "r" (old_val), "r" (new_val)
                        : "cc", "memory");
  return ret;
}
#elif defined(__riscv)
# error expected a compiler with a CAS intrinsic for RISC-V
#elif defined(__loongarch64)
# error expected a compiler with a CAS intrinsic for LoongArch64
#else
# define COMPARE_AND_SWAP_PTR(a, old, new) ((*(ptr *)(a) == TO_PTR(old)) ? (*(ptr *)(a) = TO_PTR(new), 1) : 0)
#endif
