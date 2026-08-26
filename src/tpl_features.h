#pragma once

// Public build-time capability model. Every feature is always defined to
// either 0 or 1 so ports can use ordinary #if tests without recreating the
// build system's platform logic.

#ifndef TPL_FREESTANDING
#define TPL_FREESTANDING 0
#endif

#if TPL_FREESTANDING && (!defined(EMBED) || !(EMBED))
#error "freestanding builds require embedded library(builtins)"
#endif

#ifndef TPL_FEATURE_FILESYSTEM
#define TPL_FEATURE_FILESYSTEM (!TPL_FREESTANDING)
#endif

#ifndef TPL_FEATURE_MMAP
#define TPL_FEATURE_MMAP (!TPL_FREESTANDING)
#endif

#ifndef TPL_FEATURE_NETWORK
#define TPL_FEATURE_NETWORK (!TPL_FREESTANDING)
#endif

#ifndef TPL_FEATURE_PROCESS
#define TPL_FEATURE_PROCESS (!TPL_FREESTANDING)
#endif

#ifndef TPL_FEATURE_THREADS
#define TPL_FEATURE_THREADS USE_THREADS
#endif

#ifndef TPL_FEATURE_TTY
#define TPL_FEATURE_TTY (!TPL_FREESTANDING)
#endif

#ifndef TPL_FEATURE_FFI
#define TPL_FEATURE_FFI USE_FFI
#endif

#ifndef TPL_FEATURE_TLS
#define TPL_FEATURE_TLS USE_OPENSSL
#endif

#ifndef TPL_FEATURE_REALTIME_CLOCK
#define TPL_FEATURE_REALTIME_CLOCK (!TPL_FREESTANDING)
#endif

#if TPL_FEATURE_TLS && !TPL_FEATURE_NETWORK
#error "TPL_FEATURE_TLS requires TPL_FEATURE_NETWORK"
#endif

#if USE_OPENSSL && !TPL_FEATURE_TLS
#error "USE_OPENSSL requires TPL_FEATURE_TLS"
#endif

#if USE_THREADS && !TPL_FEATURE_THREADS
#error "USE_THREADS requires TPL_FEATURE_THREADS"
#endif

#if USE_FFI && !TPL_FEATURE_FFI
#error "USE_FFI requires TPL_FEATURE_FFI"
#endif
