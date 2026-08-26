;;; asyncio.ss
;;; Copyright 2026 Cisco Systems, Inc.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; libuv-backed asynchronous I/O for the fiber scheduler.
;;;
;;; The C shim (c/asyncio.c) owns the concrete layouts of uv_loop_t,
;;; uv_handle_t, and uv_req_t.  Scheme code refers to native objects through
;;; sealed records and stable integer identifiers kept in a per-scheduler
;;; registry.  Native completions enter an owner-thread FIFO and are dispatched
;;; by the scheduler poll hook; a libuv worker callback never runs Scheme code,
;;; and the trampoline never raises.

(let ()  ; private scope: public names are assigned to their declared globals

(define-syntax aio-trace (syntax-rules () [(_ e ...) (void)]))

  (include "asyncio/ffi.ss")
  (include "asyncio/core.ss")
  (include "asyncio/network.ss")
  (include "asyncio/filesystem.ss")
  (include "asyncio/api.ss"))
