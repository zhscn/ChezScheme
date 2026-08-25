#!/bin/bash
set -e -o pipefail

export ZUO_JOBS="$(getconf _NPROCESSORS_ONLN)"
if test "$SANITIZER" = thread ; then
    export TSAN_OPTIONS=halt_on_error=1
    export CHEZ_NATIVE_FIBER_RACE_ROUNDS=8
    export CHEZ_NATIVE_FIBER_STRESS_ROUNDS=36
    export CHEZ_MAT_NAME_PREFIX=native-fiber-
    bin/zuo "$TARGET_MACHINE/mats/main.zuo" thread.mo noisy=t
    exit
elif test "$SANITIZER" = address ; then
    export ASAN_OPTIONS=detect_leaks=0:halt_on_error=1
    export CHEZ_MAT_NAME_PREFIX=native-fiber-
    bin/zuo "$TARGET_MACHINE/mats/main.zuo" thread.mo noisy=t
    exit
fi

if test "$TEST_TARGET" = ""; then
    TEST_TARGET=test-some
fi
if test "$TOOLCHAIN" = vs ; then
    MSYS_NO_PATHCONV=1 cmd.exe /c "build.bat $TARGET_MACHINE /$TEST_TARGET"
else
    make $TEST_TARGET
fi
