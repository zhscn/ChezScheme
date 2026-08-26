;;; async.ss
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

;;; Native-fiber asynchronous execution facility.
;;;
;;; Each worker owns a scheduler fiber and each task owns a VM execution
;;; context.  Waitables (timers, channels, futures, joins) are expressed as
;;; operations with try/block/wrap/nack components.  Optional tick preemption
;;; returns a running task to its scheduler at a runtime safe point.
;;; libuv-backed I/O plugs in through the scheduler's io fields (asyncio.ss).

(let ()  ; private scope: public names are assigned to their declared globals
  (include "async/core.ss")
  (include "async/operations.ss")
  (include "async/runtime.ss")
  (include "async/api.ss"))
