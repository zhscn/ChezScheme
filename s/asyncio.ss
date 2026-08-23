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

;;; libuv-backed asynchronous I/O for the fiber scheduler (ASYNC.md layer 4).
;;;
;;; The C shim (c/asyncio.c) owns the concrete layouts of uv_loop_t,
;;; uv_handle_t, and uv_req_t.  Scheme code refers to native objects through
;;; sealed records and stable integer identifiers kept in a per-scheduler
;;; registry.  Native completions enter an owner-thread FIFO and are dispatched
;;; by the scheduler poll hook; a libuv worker callback never runs Scheme code,
;;; and the trampoline never raises.

(let ()  ; private scope: public names are assigned to their declared globals

(define-syntax aio-trace (syntax-rules () [(_ e ...) (void)]))

;;; ------------------------------------------------------------ constants

;;; event kinds reported by the shim (see c/asyncio.c)
(define AIO-EV-ACCEPT 1)
(define AIO-EV-READ 2)
(define AIO-EV-WRITE 3)
(define AIO-EV-CONNECT 4)
(define AIO-EV-SHUTDOWN 5)
(define AIO-EV-FS 6)
(define AIO-EV-DNS 7)
(define AIO-EV-CLOSE 8)
(define AIO-EV-UDP-RECV 9)
(define AIO-EV-UDP-SEND 10)
(define AIO-EV-NAMEINFO 11)
(define AIO-EV-RANDOM 12)
(define AIO-EV-POLL 13)
(define AIO-EV-PROCESS 14)
(define AIO-EV-SIGNAL 15)
(define AIO-EV-FS-EVENT 16)
(define AIO-EV-FS-POLL 17)

;;; per-poll dispatch bound so a sustained completion stream cannot starve
;;; runnable tasks
(define aio-dispatch-bound 256)

;;; ------------------------------------------------------------ conditions

(define-condition-type &async-io &error
  make-async-io-condition% %async-io-condition?
  (operation %async-io-condition-operation)
  (handle %async-io-condition-handle)
  (path %async-io-condition-path)
  (code %async-io-condition-code))

;;; ------------------------------------------------------- shim loading

(define aio-loaded? #f)

;;; foreign-procedure bindings, resolved lazily from the statically linked
;;; kernel
(define aio-loop-open #f)
(define aio-set-notify #f)
(define aio-loop-run #f)
(define aio-loop-alive #f)
(define aio-loop-destroy #f)
(define aio-wakeup-init #f)
(define aio-wakeup-send #f)
(define aio-bridge-init #f)
(define aio-bridge-start #f)
(define aio-bridge-stop #f)
(define aio-handle-close #f)
(define aio-handle-is-closing #f)
(define aio-tcp-init #f)
(define aio-tcp-bind #f)
(define aio-listen-start #f)
(define aio-accept #f)
(define aio-tcp-connect #f)
(define aio-pipe-init #f)
(define aio-pipe-bind #f)
(define aio-pipe-connect #f)
(define aio-read-start #f)
(define aio-read-stop #f)
(define aio-read-copy #f)
(define aio-free #f)
(define aio-write #f)
(define aio-shutdown #f)
(define aio-udp-init #f)
(define aio-udp-bind #f)
(define aio-udp-connect #f)
(define aio-udp-recv-start #f)
(define aio-udp-recv-stop #f)
(define aio-udp-recv-copy #f)
(define aio-udp-recv-addr #f)
(define aio-udp-recv-free #f)
(define aio-udp-send #f)
(define aio-udp-address #f)
(define aio-udp-set-membership #f)
(define aio-udp-set-option #f)
(define aio-udp-set-multicast-interface #f)
(define aio-dns-lookup #f)
(define aio-dns-cancel #f)
(define aio-dns-count #f)
(define aio-dns-addr #f)
(define aio-dns-free #f)
(define aio-dns-reverse #f)
(define aio-dns-reverse-cancel #f)
(define aio-dns-reverse-copy #f)
(define aio-dns-reverse-free #f)
(define aio-random #f)
(define aio-random-cancel #f)
(define aio-random-copy #f)
(define aio-random-free #f)
(define aio-poll-init #f)
(define aio-poll-start #f)
(define aio-poll-stop #f)
(define aio-process-spawn #f)
(define aio-process-pid #f)
(define aio-process-kill #f)
(define aio-kill #f)
(define aio-process-term-signal #f)
(define aio-process-result-free #f)
(define aio-signal-init #f)
(define aio-signal-start #f)
(define aio-signal-stop #f)
(define aio-fs-event-init #f)
(define aio-fs-event-start #f)
(define aio-fs-event-stop #f)
(define aio-fs-event-result-copy #f)
(define aio-fs-event-result-free #f)
(define aio-fs-poll-init #f)
(define aio-fs-poll-start #f)
(define aio-fs-poll-stop #f)
(define aio-fs-poll-result-field #f)
(define aio-fs-poll-result-free #f)
(define aio-tty-init #f)
(define aio-tty-set-mode #f)
(define aio-tty-winsize #f)
(define aio-tty-get-vterm-state #f)
(define aio-tty-set-vterm-state #f)
(define aio-tty-reset-mode #f)
(define aio-system-u64 #f)
(define aio-system-double #f)
(define aio-system-string #f)
(define aio-uname-string #f)
(define aio-cpu-info #f)
(define aio-cpu-info-count #f)
(define aio-cpu-info-model #f)
(define aio-cpu-info-field #f)
(define aio-cpu-info-free #f)
(define aio-interface-info #f)
(define aio-interface-count #f)
(define aio-interface-name #f)
(define aio-interface-address #f)
(define aio-interface-internal #f)
(define aio-interface-physical #f)
(define aio-interface-free #f)
(define aio-rusage #f)
(define aio-rusage-field #f)
(define aio-rusage-free #f)
(define aio-loop-metric #f)
(define aio-fs-open #f)
(define aio-fs-read #f)
(define aio-fs-write #f)
(define aio-fs-close-fd #f)
(define aio-fs-close-now #f)
(define aio-fs-stat #f)
(define aio-fs-fstat #f)
(define aio-fs-unlink #f)
(define aio-fs-rename #f)
(define aio-fs-mkdir #f)
(define aio-fs-rmdir #f)
(define aio-fs-copyfile #f)
(define aio-fs-mkdtemp #f)
(define aio-fs-mkstemp #f)
(define aio-fs-scandir #f)
(define aio-fs-scandir-next #f)
(define aio-fs-opendir #f)
(define aio-fs-readdir #f)
(define aio-fs-closedir #f)
(define aio-fs-closedir-now #f)
(define aio-fs-result-ptr #f)
(define aio-fs-readdir-entry #f)
(define aio-fs-fsync #f)
(define aio-fs-fdatasync #f)
(define aio-fs-ftruncate #f)
(define aio-fs-sendfile #f)
(define aio-fs-access #f)
(define aio-fs-chmod #f)
(define aio-fs-fchmod #f)
(define aio-fs-utime #f)
(define aio-fs-futime #f)
(define aio-fs-lstat #f)
(define aio-fs-link #f)
(define aio-fs-readlink #f)
(define aio-fs-result-string-length #f)
(define aio-fs-result-string-copy #f)
(define aio-fs-result-path-length #f)
(define aio-fs-result-path-copy #f)
(define aio-fs-chown #f)
(define aio-fs-statfs #f)
(define aio-fs-statfs-field #f)
(define aio-fs-req-free #f)
(define aio-fs-cancel #f)
(define aio-fs-data #f)
(define aio-fs-buf-free #f)
(define aio-fs-stat-field #f)
(define aio-strerror-into #f)
(define aio-err-name-into #f)
(define aio-eof-code #f)
(define aio-eagain-code #f)

(define aio-resolve!
  (lambda ()
    (set! aio-loop-open (foreign-procedure "aio_loop_open" () void*))
    (set! aio-set-notify (foreign-procedure "aio_set_notify" (void* void*) void))
    (set! aio-loop-run (foreign-procedure "aio_loop_run" (void* int) int))
    (set! aio-loop-alive (foreign-procedure "aio_loop_alive" (void*) int))
    (set! aio-loop-destroy (foreign-procedure "aio_loop_destroy" (void*) int))
    (set! aio-wakeup-init (foreign-procedure "aio_wakeup_init" (void*) void*))
    (set! aio-wakeup-send (foreign-procedure "aio_wakeup_send" (void*) int))
    (set! aio-bridge-init (foreign-procedure "aio_bridge_init" (void*) void*))
    (set! aio-bridge-start (foreign-procedure "aio_bridge_start" (void* integer-64) int))
    (set! aio-bridge-stop (foreign-procedure "aio_bridge_stop" (void*) int))
    (set! aio-handle-close (foreign-procedure "aio_handle_close" (void*) void))
    (set! aio-handle-is-closing (foreign-procedure "aio_handle_is_closing" (void*) int))
    (set! aio-tcp-init (foreign-procedure "aio_tcp_init" (void* integer-64) void*))
    (set! aio-tcp-bind (foreign-procedure "aio_tcp_bind" (void* string integer-64) int))
    (set! aio-listen-start (foreign-procedure "aio_listen_start" (void* integer-64) int))
    (set! aio-accept (foreign-procedure "aio_accept" (void* void*) int))
    (set! aio-tcp-connect (foreign-procedure "aio_tcp_connect" (void* string integer-64 integer-64) int))
    (set! aio-pipe-init (foreign-procedure "aio_pipe_init" (void* integer-64) void*))
    (set! aio-pipe-bind (foreign-procedure "aio_pipe_bind" (void* string) int))
    (set! aio-pipe-connect (foreign-procedure "aio_pipe_connect" (void* string integer-64) int))
    (set! aio-read-start (foreign-procedure "aio_read_start" (void*) int))
    (set! aio-read-stop (foreign-procedure "aio_read_stop" (void*) int))
    (set! aio-read-copy (foreign-procedure "aio_read_copy" (void* u8* integer-64) void))
    (set! aio-free (foreign-procedure "aio_free" (void*) void))
    (set! aio-write (foreign-procedure "aio_write" (void* u8* integer-64 integer-64) int))
    (set! aio-shutdown (foreign-procedure "aio_shutdown" (void* integer-64) int))
    (set! aio-udp-init (foreign-procedure "aio_udp_init" (void* integer-64) void*))
    (set! aio-udp-bind (foreign-procedure "aio_udp_bind" (void* string integer-64 int) int))
    (set! aio-udp-connect (foreign-procedure "aio_udp_connect" (void* string integer-64) int))
    (set! aio-udp-recv-start (foreign-procedure "aio_udp_recv_start" (void*) int))
    (set! aio-udp-recv-stop (foreign-procedure "aio_udp_recv_stop" (void*) int))
    (set! aio-udp-recv-copy (foreign-procedure "aio_udp_recv_copy" (void* u8* integer-64) void))
    (set! aio-udp-recv-addr (foreign-procedure "aio_udp_recv_addr" (void* u8* integer-64) integer-64))
    (set! aio-udp-recv-free (foreign-procedure "aio_udp_recv_free" (void*) void))
    (set! aio-udp-send (foreign-procedure "aio_udp_send" (void* u8* integer-64 string integer-64 integer-64) int))
    (set! aio-udp-address (foreign-procedure "aio_udp_address" (void* int u8* integer-64) integer-64))
    (set! aio-udp-set-membership (foreign-procedure "aio_udp_set_membership" (void* string string string int) int))
    (set! aio-udp-set-option (foreign-procedure "aio_udp_set_option" (void* int integer-64) int))
    (set! aio-udp-set-multicast-interface (foreign-procedure "aio_udp_set_multicast_interface" (void* string) int))
    (set! aio-dns-lookup (foreign-procedure "aio_dns_lookup" (void* string string integer-64) integer-64))
    (set! aio-dns-cancel (foreign-procedure "aio_dns_cancel" (void*) int))
    (set! aio-dns-count (foreign-procedure "aio_dns_count" (void*) integer-64))
    (set! aio-dns-addr (foreign-procedure "aio_dns_addr" (void* integer-64 u8* integer-64) integer-64))
    (set! aio-dns-free (foreign-procedure "aio_dns_free" (void*) void))
    (set! aio-dns-reverse (foreign-procedure "aio_dns_reverse" (void* string integer-64 int integer-64) integer-64))
    (set! aio-dns-reverse-cancel (foreign-procedure "aio_dns_reverse_cancel" (void*) int))
    (set! aio-dns-reverse-copy (foreign-procedure "aio_dns_reverse_copy" (void* int u8* integer-64) integer-64))
    (set! aio-dns-reverse-free (foreign-procedure "aio_dns_reverse_free" (void*) void))
    (set! aio-random (foreign-procedure "aio_random" (void* integer-64 integer-64) integer-64))
    (set! aio-random-cancel (foreign-procedure "aio_random_cancel" (void*) int))
    (set! aio-random-copy (foreign-procedure "aio_random_copy" (void* u8*) void))
    (set! aio-random-free (foreign-procedure "aio_random_free" (void*) void))
    (set! aio-poll-init (foreign-procedure "aio_poll_init" (void* integer-64 integer-64) void*))
    (set! aio-poll-start (foreign-procedure "aio_poll_start" (void* int) int))
    (set! aio-poll-stop (foreign-procedure "aio_poll_stop" (void*) int))
    (set! aio-process-spawn
      (foreign-procedure "aio_process_spawn"
        (void* integer-64 string u8* integer-64 u8* integer-64 int string int
         int integer-64 void* int integer-64 void* int integer-64 void*)
        integer-64))
    (set! aio-process-pid (foreign-procedure "aio_process_pid" (void*) integer-64))
    (set! aio-process-kill (foreign-procedure "aio_process_kill" (void* int) int))
    (set! aio-kill (foreign-procedure "aio_kill" (integer-64 int) int))
    (set! aio-process-term-signal (foreign-procedure "aio_process_term_signal" (void*) integer-64))
    (set! aio-process-result-free (foreign-procedure "aio_process_result_free" (void*) void))
    (set! aio-signal-init (foreign-procedure "aio_signal_init" (void* integer-64) void*))
    (set! aio-signal-start (foreign-procedure "aio_signal_start" (void* int int) int))
    (set! aio-signal-stop (foreign-procedure "aio_signal_stop" (void*) int))
    (set! aio-fs-event-init (foreign-procedure "aio_fs_event_init" (void* integer-64) void*))
    (set! aio-fs-event-start (foreign-procedure "aio_fs_event_start" (void* string int) int))
    (set! aio-fs-event-stop (foreign-procedure "aio_fs_event_stop" (void*) int))
    (set! aio-fs-event-result-copy (foreign-procedure "aio_fs_event_result_copy" (void* u8* integer-64) int))
    (set! aio-fs-event-result-free (foreign-procedure "aio_fs_event_result_free" (void*) void))
    (set! aio-fs-poll-init (foreign-procedure "aio_fs_poll_init" (void* integer-64) void*))
    (set! aio-fs-poll-start (foreign-procedure "aio_fs_poll_start" (void* string integer-64) int))
    (set! aio-fs-poll-stop (foreign-procedure "aio_fs_poll_stop" (void*) int))
    (set! aio-fs-poll-result-field (foreign-procedure "aio_fs_poll_result_field" (void* int integer-64) integer-64))
    (set! aio-fs-poll-result-free (foreign-procedure "aio_fs_poll_result_free" (void*) void))
    (set! aio-tty-init (foreign-procedure "aio_tty_init" (void* integer-64 integer-64) void*))
    (set! aio-tty-set-mode (foreign-procedure "aio_tty_set_mode" (void* int) int))
    (set! aio-tty-winsize (foreign-procedure "aio_tty_winsize" (void* int) int))
    (set! aio-tty-get-vterm-state (foreign-procedure "aio_tty_get_vterm_state" () int))
    (set! aio-tty-set-vterm-state (foreign-procedure "aio_tty_set_vterm_state" (int) void))
    (set! aio-tty-reset-mode (foreign-procedure "aio_tty_reset_mode" () void))
    (set! aio-system-u64 (foreign-procedure "aio_system_u64" (int) unsigned-64))
    (set! aio-system-double (foreign-procedure "aio_system_double" (int) double))
    (set! aio-system-string (foreign-procedure "aio_system_string" (int u8* integer-64) int))
    (set! aio-uname-string (foreign-procedure "aio_uname_string" (int u8* integer-64) int))
    (set! aio-cpu-info (foreign-procedure "aio_cpu_info" () void*))
    (set! aio-cpu-info-count (foreign-procedure "aio_cpu_info_count" (void*) integer-64))
    (set! aio-cpu-info-model (foreign-procedure "aio_cpu_info_model" (void* integer-64 u8* integer-64) int))
    (set! aio-cpu-info-field (foreign-procedure "aio_cpu_info_field" (void* integer-64 int) unsigned-64))
    (set! aio-cpu-info-free (foreign-procedure "aio_cpu_info_free" (void*) void))
    (set! aio-interface-info (foreign-procedure "aio_interface_info" () void*))
    (set! aio-interface-count (foreign-procedure "aio_interface_count" (void*) integer-64))
    (set! aio-interface-name (foreign-procedure "aio_interface_name" (void* integer-64 u8* integer-64) int))
    (set! aio-interface-address (foreign-procedure "aio_interface_address" (void* integer-64 int u8* integer-64) integer-64))
    (set! aio-interface-internal (foreign-procedure "aio_interface_internal" (void* integer-64) int))
    (set! aio-interface-physical (foreign-procedure "aio_interface_physical" (void* integer-64 u8*) int))
    (set! aio-interface-free (foreign-procedure "aio_interface_free" (void*) void))
    (set! aio-rusage (foreign-procedure "aio_rusage" () void*))
    (set! aio-rusage-field (foreign-procedure "aio_rusage_field" (void* int) integer-64))
    (set! aio-rusage-free (foreign-procedure "aio_rusage_free" (void*) void))
    (set! aio-loop-metric (foreign-procedure "aio_loop_metric" (void* int) integer-64))
    (set! aio-fs-open (foreign-procedure "aio_fs_open" (void* string int integer-64 integer-64) integer-64))
    (set! aio-fs-read (foreign-procedure "aio_fs_read" (void* integer-64 integer-64 integer-64 integer-64) integer-64))
    (set! aio-fs-write (foreign-procedure "aio_fs_write" (void* integer-64 u8* integer-64 integer-64 integer-64) integer-64))
    (set! aio-fs-close-fd (foreign-procedure "aio_fs_close_fd" (void* integer-64 integer-64) integer-64))
    (set! aio-fs-close-now (foreign-procedure "aio_fs_close_now" (integer-64) int))
    (set! aio-fs-stat (foreign-procedure "aio_fs_stat" (void* string integer-64) integer-64))
    (set! aio-fs-fstat (foreign-procedure "aio_fs_fstat" (void* integer-64 integer-64) integer-64))
    (set! aio-fs-unlink (foreign-procedure "aio_fs_unlink" (void* string integer-64) integer-64))
    (set! aio-fs-rename (foreign-procedure "aio_fs_rename" (void* string string integer-64) integer-64))
    (set! aio-fs-mkdir (foreign-procedure "aio_fs_mkdir" (void* string integer-64 integer-64) integer-64))
    (set! aio-fs-rmdir (foreign-procedure "aio_fs_rmdir" (void* string integer-64) integer-64))
    (set! aio-fs-copyfile (foreign-procedure "aio_fs_copyfile" (void* string string int integer-64) integer-64))
    (set! aio-fs-mkdtemp (foreign-procedure "aio_fs_mkdtemp" (void* string integer-64) integer-64))
    (set! aio-fs-mkstemp (foreign-procedure "aio_fs_mkstemp" (void* string integer-64) integer-64))
    (set! aio-fs-scandir (foreign-procedure "aio_fs_scandir" (void* string integer-64) integer-64))
    (set! aio-fs-scandir-next (foreign-procedure "aio_fs_scandir_next" (void* u8* integer-64) int))
    (set! aio-fs-opendir (foreign-procedure "aio_fs_opendir" (void* string integer-64) integer-64))
    (set! aio-fs-readdir (foreign-procedure "aio_fs_readdir" (void* void* integer-64 integer-64) integer-64))
    (set! aio-fs-closedir (foreign-procedure "aio_fs_closedir" (void* void* integer-64) integer-64))
    (set! aio-fs-closedir-now (foreign-procedure "aio_fs_closedir_now" (void*) int))
    (set! aio-fs-result-ptr (foreign-procedure "aio_fs_result_ptr" (void*) void*))
    (set! aio-fs-readdir-entry (foreign-procedure "aio_fs_readdir_entry" (void* integer-64 u8* integer-64) int))
    (set! aio-fs-fsync (foreign-procedure "aio_fs_fsync" (void* integer-64 integer-64) integer-64))
    (set! aio-fs-fdatasync (foreign-procedure "aio_fs_fdatasync" (void* integer-64 integer-64) integer-64))
    (set! aio-fs-ftruncate (foreign-procedure "aio_fs_ftruncate" (void* integer-64 integer-64 integer-64) integer-64))
    (set! aio-fs-sendfile (foreign-procedure "aio_fs_sendfile" (void* integer-64 integer-64 integer-64 integer-64 integer-64) integer-64))
    (set! aio-fs-access (foreign-procedure "aio_fs_access" (void* string int integer-64) integer-64))
    (set! aio-fs-chmod (foreign-procedure "aio_fs_chmod" (void* string integer-64 integer-64) integer-64))
    (set! aio-fs-fchmod (foreign-procedure "aio_fs_fchmod" (void* integer-64 integer-64 integer-64) integer-64))
    (set! aio-fs-utime (foreign-procedure "aio_fs_utime" (void* string double double int integer-64) integer-64))
    (set! aio-fs-futime (foreign-procedure "aio_fs_futime" (void* integer-64 double double integer-64) integer-64))
    (set! aio-fs-lstat (foreign-procedure "aio_fs_lstat" (void* string integer-64) integer-64))
    (set! aio-fs-link (foreign-procedure "aio_fs_link" (void* string string int int integer-64) integer-64))
    (set! aio-fs-readlink (foreign-procedure "aio_fs_readlink" (void* string int integer-64) integer-64))
    (set! aio-fs-result-string-length (foreign-procedure "aio_fs_result_string_length" (void*) integer-64))
    (set! aio-fs-result-string-copy (foreign-procedure "aio_fs_result_string_copy" (void* u8* integer-64) int))
    (set! aio-fs-result-path-length (foreign-procedure "aio_fs_result_path_length" (void*) integer-64))
    (set! aio-fs-result-path-copy (foreign-procedure "aio_fs_result_path_copy" (void* u8* integer-64) int))
    (set! aio-fs-chown (foreign-procedure "aio_fs_chown" (void* string integer-64 integer-64 integer-64 int integer-64) integer-64))
    (set! aio-fs-statfs (foreign-procedure "aio_fs_statfs" (void* string integer-64) integer-64))
    (set! aio-fs-statfs-field (foreign-procedure "aio_fs_statfs_field" (void* int) unsigned-64))
    (set! aio-fs-req-free (foreign-procedure "aio_fs_req_free" (void*) void))
    (set! aio-fs-cancel (foreign-procedure "aio_fs_cancel" (void*) int))
    (set! aio-fs-data (foreign-procedure "aio_fs_data" (void*) void*))
    (set! aio-fs-buf-free (foreign-procedure "aio_fs_buf_free" (void*) void))
    (set! aio-fs-stat-field (foreign-procedure "aio_fs_stat_field" (void* integer-64) integer-64))
    (set! aio-strerror-into (foreign-procedure "aio_strerror_into" (integer-64 u8* integer-64) void))
    (set! aio-err-name-into (foreign-procedure "aio_err_name_into" (integer-64 u8* integer-64) void))
    (set! aio-eof-code (foreign-procedure "aio_eof_code" () integer-64))
    (set! aio-eagain-code (foreign-procedure "aio_eagain_code" () integer-64))))

(define aio-resolve-kernel!
  (lambda ()
    (unless aio-loaded?
      (aio-resolve!)
      (set! aio-loaded? #t))))

;;; ------------------------------------------------------------ records

(define-record-type (aio-completion make-aio-completion aio-completion?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable id)
    (immutable kind)
    (immutable status)
    (immutable aux)
    (mutable next)))

(define-record-type (aio-completion-queue
                      make-aio-completion-queue%
                      aio-completion-queue?)
  (nongenerative)
  (sealed #t)
  (fields (mutable head) (mutable tail)))

(define make-aio-completion-queue
  (lambda () (make-aio-completion-queue% #f #f)))

(define aio-completion-queue-empty?
  (lambda (queue) (not (aio-completion-queue-head queue))))

(define aio-completion-queue-push!
  (lambda (queue completion)
    (let ([tail (aio-completion-queue-tail queue)])
      (if tail
          (aio-completion-next-set! tail completion)
          (aio-completion-queue-head-set! queue completion))
      (aio-completion-queue-tail-set! queue completion))))

(define aio-completion-queue-pop!
  (lambda (queue)
    (let ([head (aio-completion-queue-head queue)])
      (when head
        (let ([next (aio-completion-next head)])
          (aio-completion-queue-head-set! queue next)
          (unless next (aio-completion-queue-tail-set! queue #f))
          (aio-completion-next-set! head #f)))
      head)))

;;; Intrusive owner-locked queues are used for I/O waiters and serialized
;;; filesystem operations.  Cancellation unlinks the exact node in O(1).
(define-record-type (aio-queue-node make-aio-queue-node aio-queue-node?)
  (nongenerative)
  (sealed #t)
  (fields (immutable value) (mutable owner) (mutable previous) (mutable next)))

(define-record-type (aio-queue make-aio-queue% aio-queue?)
  (nongenerative)
  (sealed #t)
  (fields (mutable head) (mutable tail)))

(define make-aio-queue
  (lambda () (make-aio-queue% #f #f)))

(define aio-queue-empty?
  (lambda (queue) (not (aio-queue-head queue))))

(define aio-queue-push!
  (lambda (queue value)
    (let* ([tail (aio-queue-tail queue)]
           [node (make-aio-queue-node value queue tail #f)])
      (if tail
          (aio-queue-node-next-set! tail node)
          (aio-queue-head-set! queue node))
      (aio-queue-tail-set! queue node)
      node)))

(define aio-queue-remove!
  (lambda (queue node)
    (and (eq? (aio-queue-node-owner node) queue)
         (let ([previous (aio-queue-node-previous node)]
               [next (aio-queue-node-next node)])
           (if previous
               (aio-queue-node-next-set! previous next)
               (aio-queue-head-set! queue next))
           (if next
               (aio-queue-node-previous-set! next previous)
               (aio-queue-tail-set! queue previous))
           (aio-queue-node-owner-set! node #f)
           (aio-queue-node-previous-set! node #f)
           (aio-queue-node-next-set! node #f)
           #t))))

(define aio-queue-pop!
  (lambda (queue)
    (let ([node (aio-queue-head queue)])
      (and node
           (begin
             (aio-queue-remove! queue node)
             (aio-queue-node-value node))))))

(define aio-queue-peek
  (lambda (queue)
    (let ([node (aio-queue-head queue)])
      (and node (aio-queue-node-value node)))))

(define aio-queue-drain!
  (lambda (queue)
    (let loop ([values '()])
      (let ([value (aio-queue-pop! queue)])
        (if value (loop (cons value values)) (reverse values))))))

(define aio-queue-pop-live!
  (lambda (queue)
    (let loop ()
      (let ([waiter (aio-queue-pop! queue)])
        (and waiter
             (if (aio-waiter-dead? (car waiter)) (loop) waiter))))))

(define aio-queue-peek-live
  (lambda (queue)
    (let loop ()
      (let ([waiter (aio-queue-peek queue)])
        (and waiter
             (if (aio-waiter-dead? (car waiter))
                 (begin (aio-queue-pop! queue) (loop))
                 waiter))))))

;;; One per scheduler that has touched I/O.  Requests are in-flight native
;;; operations keyed by request id; handles are live listeners/streams keyed
;;; by handle id.  The tables are guarded because a cancellation nack can run
;;; on a thread other than the scheduler's.
(define-record-type (aio-state make-aio-state aio-state?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable owner)             ; scheduler that drives this loop
    (immutable loop)              ; void* aio_loop_t
    (immutable wakeup)            ; void* uv_async_t
    (immutable bridge)            ; void* uv_timer_t
    (mutable next-id)
    (immutable requests)          ; id -> aio-req
    (immutable requests-mutex)
    (immutable handles)           ; id -> weak-cons wrapper #t
    (immutable files)             ; fd -> weak-cons async-file #t
    (immutable directories)       ; native pointer -> weak-cons async-directory #t
    (immutable completions)       ; owner-only FIFO of native completions
    (mutable commands)            ; owner-thread thunks, newest first
    (immutable command-mutex)     ; also guards closing and wakeup lifetime
    (mutable stop-set)            ; streams that may need uv_read_stop
    (immutable stop-mutex)
    (mutable closing?)
    (immutable guardian)
    (immutable file-guardian)
    (immutable directory-guardian)))

(define-record-type (aio-req make-aio-req aio-req?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable kind)              ; 'write 'connect 'shutdown 'fs 'dns
    (immutable handle)            ; owning stream/file, or #f
    (mutable deliver)             ; deliver closure or #f
    (immutable cancel-data)       ; ctx pointer for uv_cancel, or #f
    (immutable finish)            ; canceled? status aux -> payload or #f
    (mutable canceled?)))

;;; wrappers for listeners and streams; the id doubles as the native handle's
;;; data field and never changes, so a recycled descriptor cannot be confused
;;; with a stale wrapper
(define-record-type (aio-handle make-aio-handle aio-handle?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable id)
    (immutable handle)            ; void*
    (immutable kind)              ; 'tcp-stream 'pipe-stream 'tcp-listener 'pipe-listener
    (immutable state)             ; owning aio-state
    (immutable path)              ; descriptive string or #f
    (mutable port-owned?)
    (immutable mutex)
    (mutable closing?)
    (mutable closed?)             ; close callback has run
    (immutable read-queue)        ; aio-queue of (ss . deliver)
    (mutable reading?)
    (mutable eof?)
    (immutable accept-queue)      ; aio-queue of (ss . deliver)
    (mutable result)))            ; process exit result, or #f

(define-record-type (async-file make-async-file% %async-file?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable fd)
    (immutable path)
    (immutable state)
    (mutable port-owned?)
    (mutable offset)
    (mutable closed?)
    (immutable mutex)
    (mutable busy?)
    (immutable queue)))

(define-record-type (async-directory make-async-directory% %async-directory?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable pointer)
    (immutable path)
    (immutable state)
    (mutable closed?)
    (immutable mutex)
    (mutable busy?)
    (immutable queue)))

;;; ------------------------------------------------- notify trampoline

(define aio-debug-invariants?
  (let ([v (getenv "CHEZ_ASYNC_CHECK_INVARIANTS")])
    (and v (not (member v '("" "0" "false" "no"))))))

(define aio-invariant
  (lambda (ok? message object)
    (when (and aio-debug-invariants? (not ok?))
      ($oops 'async-io-invariant "~a: ~s" message object))))

;;; Runs inside uv_run on the scheduler thread.  Enqueues the completion and
;;; nothing more; a condition is swallowed rather than escaping through C.
(define aio-notify-trampoline
  (let ([p (foreign-callable
             (lambda (lp id kind status aux)
               (guard (c [else (void)])
                 (let* ([sched (current-async-scheduler)]
                        [st (and sched ($async-scheduler-io-state sched))])
                   (when (and st (= lp (aio-state-loop st)))
                     (aio-completion-queue-push!
                       (aio-state-completions st)
                       (make-aio-completion id kind status aux #f))))))
             (void* integer-64 integer-64 integer-64 void*)
             void)])
    (lock-object p)
    p))

;;; ------------------------------------------------------------ helpers

(define aio-next-id
  (lambda (st)
    (aio-debug-check-owner! st)
    (let ([id (aio-state-next-id st)])
      (aio-state-next-id-set! st (fx+ id 1))
      id)))

(define aio-waiter-dead?
  (lambda (ss) (not ($async-sync-state-live? ss))))

;;; Deliver a single event to the oldest live waiter.  A cancellation can win
;;; after the waiter is unlinked, so retry until a delivery claims its sync
;;; state or the queue becomes empty.
(define aio-deliver-one-waiter!
  (lambda (h queue payload)
    (let loop ()
      (let ([waiter
             (with-mutex (aio-handle-mutex h)
               (aio-queue-pop-live! queue))])
        (and waiter
             (or ((cdr waiter) payload) (loop)))))))

(define aio-io-condition
  (lambda (operation handle path code)
    (make-async-io-condition% operation handle path code)))

(define aio-closed-condition
  (lambda (operation handle)
    (make-async-io-condition% operation handle (aio-handle-path handle) 'closed)))

(define aio-debug-check-owner!
  (lambda (st)
    (when aio-debug-invariants?
      (let ([owner (aio-state-owner st)])
        (aio-invariant (eq? (current-async-scheduler) owner)
          "libuv loop operation ran under a foreign scheduler" st)
        (aio-invariant ($async-scheduler-owner-thread? owner)
          "libuv loop operation ran on a foreign thread" st)))))

(define bv->cstring
  (lambda (bv)
    (let* ([n (bytevector-length bv)]
           [len (let loop ([i 0])
                  (if (or (fx= i n) (fx= (bytevector-u8-ref bv i) 0))
                      i
                      (loop (fx+ i 1))))]
           [s (make-string len)])
      (do ([i 0 (fx+ i 1)]) ((fx= i len) s)
        (string-set! s i (integer->char (bytevector-u8-ref bv i)))))))

(define aio-register-handle!
  (lambda (st w)
    (aio-debug-check-owner! st)
    (aio-invariant (eq? (aio-handle-state w) st)
      "handle registered with a foreign loop" w)
    (aio-invariant
      (not (hashtable-ref (aio-state-handles st) (aio-handle-id w) #f))
      "handle id was registered twice" w)
    (hashtable-set! (aio-state-handles st) (aio-handle-id w) (weak-cons w #t))
    ((aio-state-guardian st) w)))

(define aio-register-file!
  (lambda (st f)
    (aio-debug-check-owner! st)
    (aio-invariant (eq? (async-file-state f) st)
      "file registered with a foreign loop" f)
    (aio-invariant
      (not (hashtable-ref (aio-state-files st) (async-file-fd f) #f))
      "file descriptor was registered twice" f)
    (hashtable-set! (aio-state-files st) (async-file-fd f) (weak-cons f #t))
    ((aio-state-file-guardian st) f)
    f))

(define aio-unregister-file!
  (lambda (f)
    (aio-debug-check-owner! (async-file-state f))
    (hashtable-delete! (aio-state-files (async-file-state f))
      (async-file-fd f))))

(define aio-register-directory!
  (lambda (st d)
    (aio-debug-check-owner! st)
    (aio-invariant (eq? (async-directory-state d) st)
      "directory registered with a foreign loop" d)
    (hashtable-set! (aio-state-directories st) (async-directory-pointer d)
      (weak-cons d #t))
    ((aio-state-directory-guardian st) d)
    d))

(define aio-unregister-directory!
  (lambda (d)
    (aio-debug-check-owner! (async-directory-state d))
    (hashtable-delete! (aio-state-directories (async-directory-state d))
      (async-directory-pointer d))))

(define aio-lookup-handle
  (lambda (st id)
    (let ([p (hashtable-ref (aio-state-handles st) id #f)])
      (and p
           (let ([w (car p)])
             (and (not (bwp-object? w)) w))))))

;;; register a request; finish runs exactly once at dispatch, canceled or
;;; not, and returns a payload to deliver or #f to stay silent
(define aio-register-request!
  (lambda (st id req)
    (aio-debug-check-owner! st)
    (with-mutex (aio-state-requests-mutex st)
      (aio-invariant (not (hashtable-ref (aio-state-requests st) id #f))
        "native request id was registered twice" id)
      (let ([handle (aio-req-handle req)])
        (when (aio-handle? handle)
          (aio-invariant (eq? (aio-handle-state handle) st)
            "native request uses a handle from another loop" req))
        (when (%async-file? handle)
          (aio-invariant (eq? (async-file-state handle) st)
            "native request uses a file from another loop" req)))
      (hashtable-set! (aio-state-requests st) id req))))

;;; Native libuv objects are touched only by their loop owner.  Foreign
;;; threads enqueue identities or Scheme data and use uv_async_send solely as
;;; the wakeup mechanism.
(define aio-submit-command!
  (lambda (st command)
    (let ([accepted?
           (with-mutex (aio-state-command-mutex st)
             (if (aio-state-closing? st)
                 #f
                 (begin
                   (aio-state-commands-set! st
                     (cons command (aio-state-commands st)))
                   #t)))])
      (when accepted?
        (aio-wakeup-send (aio-state-loop st)))
      accepted?)))

(define aio-drain-commands!
  (lambda (st)
    (aio-debug-check-owner! st)
    (let ([commands
           (with-mutex (aio-state-command-mutex st)
             (let ([commands (reverse (aio-state-commands st))])
               (aio-state-commands-set! st '())
               commands))])
      (for-each (lambda (command) (command)) commands))))

(define aio-run-on-owner!
  (lambda (st command)
    (if (eq? (current-async-scheduler) (aio-state-owner st))
        (begin
          (aio-debug-check-owner! st)
          (command)
          #t)
        (aio-submit-command! st command))))

(define aio-atomic-box-ref
  (lambda (b)
    (let loop ()
      (let ([v (unbox b)])
        (if (box-cas! b v v) v (loop))))))

(define aio-atomic-box-set-once!
  (lambda (b v)
    (unless (box-cas! b #f v)
      ($oops 'async-io "internal request identity was published twice"))))

(define aio-atomic-box-flag!
  (lambda (b)
    (unless (aio-atomic-box-ref b)
      (box-cas! b #f #t))
    (void)))

;;; finish proc for requests whose native context is a uv_fs_t wrapper
(define aio-fs-finish
  (lambda (gen canceled-gen)
    (lambda (canceled? status aux)
      (let ([payload #f])
        (dynamic-wind
          (lambda () (void))
          (lambda ()
            (if canceled?
                (canceled-gen status aux)
                (set! payload (gen status aux))))
          (lambda ()
            (aio-fs-buf-free aux)
            (aio-fs-req-free aux)))
        payload))))

;;; finish proc for dns requests
(define aio-dns-finish
  (lambda (gen)
    (lambda (canceled? status aux)
      (let ([payload (and (not canceled?) (gen status aux))])
        (aio-dns-free aux)
        payload))))

;;; finish proc for requests whose context the shim has already released
(define aio-plain-finish
  (lambda (gen)
    (lambda (canceled? status aux)
      (and (not canceled?) (gen status aux)))))

;;; cancellation of a waiting request: logical cancellation is authoritative;
;;; uv_cancel is used only for the request types that support it
(define aio-cancel-request!
  (lambda (st id)
    (let ([found?
           (with-mutex (aio-state-requests-mutex st)
             (let ([req (hashtable-ref (aio-state-requests st) id #f)])
               (when req (aio-req-canceled?-set! req #t))
               (and req #t)))])
      (when found?
        (aio-submit-command! st
          (lambda ()
            ;; Completion and this command are both dispatched by the loop
            ;; owner, so cancel-data cannot be freed between lookup and use.
            (let ([req
                   (with-mutex (aio-state-requests-mutex st)
                     (hashtable-ref (aio-state-requests st) id #f))])
              (when req
                (let ([cd (aio-req-cancel-data req)])
                  (when cd
                    (case (aio-req-kind req)
                      [(fs) (aio-fs-cancel cd)]
                      [(dns) (aio-dns-cancel cd)]
                      [(nameinfo) (aio-dns-reverse-cancel cd)]
                      [(random) (aio-random-cancel cd)]
                      [else (void)])))))))))))

;;; ------------------------------------------------------------- dispatch

(define aio-dispatch-event
  (lambda (st id kind status aux)
    (aio-debug-check-owner! st)
    (cond
      [(fx= kind AIO-EV-READ) (aio-on-read st id status aux)]
      [(fx= kind AIO-EV-ACCEPT) (aio-on-accept st id status)]
      [(fx= kind AIO-EV-CLOSE) (aio-on-close st id)]
      [(fx= kind AIO-EV-UDP-RECV) (aio-on-udp-recv st id status aux)]
      [(fx= kind AIO-EV-POLL) (aio-on-poll st id status aux)]
      [(fx= kind AIO-EV-PROCESS) (aio-on-process-exit st id status aux)]
      [(or (fx= kind AIO-EV-SIGNAL)
           (fx= kind AIO-EV-FS-EVENT)
           (fx= kind AIO-EV-FS-POLL))
       (aio-on-watch st id kind status aux)]
      [else (aio-on-request st id status aux)])))

(define aio-on-request
  (lambda (st id status aux)
    (let ([req
           (with-mutex (aio-state-requests-mutex st)
             (let ([req (hashtable-ref (aio-state-requests st) id #f)])
               (when req (hashtable-delete! (aio-state-requests st) id))
               req))])
      (aio-invariant req "completion referenced an unknown request id" id)
      (when req
        (let* ([canceled? (or (aio-req-canceled? req) (aio-state-closing? st))]
               [payload ((aio-req-finish req) canceled? status aux)])
          (when payload
            (let ([d (aio-req-deliver req)])
              (when d (d payload)))))))))

(define aio-read-payload
  (lambda (h status aux)
    (cond
      [(fx> status 0)
       (let ([bv (make-bytevector status)])
         (aio-read-copy aux bv status)
         (aio-free aux)
         (cons 'values (list bv)))]
      [(fx= status (aio-eof-code))
       (aio-handle-eof?-set! h #t)
       (cons 'values (list #!eof))]
      [else
       (cons 'raise (aio-io-condition 'read h (aio-handle-path h) status))])))

(define aio-on-read
  (lambda (st id status aux)
    (let ([h (aio-lookup-handle st id)])
      (cond
        [(not h) (when (fx> status 0) (aio-free aux))]
        [else
         (let ([deliveries '()] [free-aux? #f])
           (with-mutex (aio-handle-mutex h)
             (aio-handle-reading?-set! h #f)
             (let ([queue (aio-handle-read-queue h)])
               (cond
                 [(or (aio-state-closing? st) (aio-handle-closing? h))
                  (set! free-aux? (fx> status 0))]
                 [(fx= status (aio-eof-code))
                  (aio-handle-eof?-set! h #t)
                  (set! deliveries
                    (map (lambda (w)
                           (cons (cdr w) (cons 'values (list #!eof))))
                      (aio-queue-drain! queue)))]
                 [else
                  (let ([waiter (aio-queue-pop-live! queue)])
                    (if waiter
                        (set! deliveries
                          (list (cons (cdr waiter)
                                  (aio-read-payload h status aux))))
                        (set! free-aux? (fx> status 0))))]))
             ;; arm the next reader, if any
           (when (and (not (aio-handle-reading? h))
                      (not (aio-handle-closing? h))
                      (not (aio-handle-eof? h))
                      (not (aio-queue-empty? (aio-handle-read-queue h))))
             (aio-handle-reading?-set! h #t)
               (aio-read-start (aio-handle-handle h))))
           (when free-aux? (aio-free aux))
           (for-each
             (lambda (d)
               (unless ((car d) (cdr d))
                 (unless (fx= status (aio-eof-code))
                   (aio-deliver-one-waiter! h
                     (aio-handle-read-queue h) (cdr d)))))
             deliveries))]))))

(define aio-udp-address-values
  (lambda (encoded buf)
    (values (bv->cstring buf)
            (fxmod encoded 65536)
            (quotient encoded 65536))))

(define aio-udp-recv-payload
  (lambda (h status aux)
    (if (fx>= status 0)
        (let ([bv (make-bytevector status)] [addr (make-bytevector 64)])
          (aio-udp-recv-copy aux bv status)
          (let ([encoded (aio-udp-recv-addr aux addr 64)])
            (aio-udp-recv-free aux)
            (if (fx< encoded 0)
                (cons 'raise
                  (aio-io-condition 'udp-receive h (aio-handle-path h)
                    encoded))
                (let-values ([(host port family)
                              (aio-udp-address-values encoded addr)])
                  (cons 'values (list bv host port family))))))
        (cons 'raise
          (aio-io-condition 'udp-receive h (aio-handle-path h) status)))))

(define aio-on-udp-recv
  (lambda (st id status aux)
    (let ([h (aio-lookup-handle st id)])
      (if (not h)
          (when aux (aio-udp-recv-free aux))
          (let ([delivery #f] [free? #f])
            (with-mutex (aio-handle-mutex h)
              (aio-handle-reading?-set! h #f)
              (let ([waiter (aio-queue-pop-live! (aio-handle-read-queue h))])
                (if (or (aio-handle-closing? h) (not waiter))
                    (set! free? (and aux #t))
                    (set! delivery
                      (cons (cdr waiter) (aio-udp-recv-payload h status aux)))))
              (when (and (not (aio-handle-closing? h))
                         (not (aio-queue-empty? (aio-handle-read-queue h))))
                (aio-handle-reading?-set! h #t)
                (aio-udp-recv-start (aio-handle-handle h))))
            (when free? (aio-udp-recv-free aux))
            (when delivery
              (unless ((car delivery) (cdr delivery))
                (aio-deliver-one-waiter! h
                  (aio-handle-read-queue h) (cdr delivery)))))))))

(define aio-poll-event-list
  (lambda (bits)
    (let ([events '()])
      (when (fxlogtest bits 1) (set! events (cons 'readable events)))
      (when (fxlogtest bits 2) (set! events (cons 'writable events)))
      (when (fxlogtest bits 4) (set! events (cons 'disconnect events)))
      (when (fxlogtest bits 8) (set! events (cons 'prioritized events)))
      (reverse events))))

(define aio-on-poll
  (lambda (st id status aux)
    (let ([h (aio-lookup-handle st id)])
      (when h
        (let ([waiter #f])
          (with-mutex (aio-handle-mutex h)
            (aio-handle-reading?-set! h #f)
            (set! waiter (aio-queue-pop-live! (aio-handle-read-queue h))))
          (when (and waiter (not (aio-waiter-dead? (car waiter))))
            ((cdr waiter)
             (if (fx< status 0)
                 (cons 'raise
                   (aio-io-condition 'fd-poll h (aio-handle-path h) status))
                 (cons 'values (list (aio-poll-event-list status)))))))))))

(define aio-on-process-exit
  (lambda (st id status aux)
    (let ([process (aio-lookup-handle st id)])
      (let ([term-signal (aio-process-term-signal aux)])
        (when aux (aio-process-result-free aux))
        (when process
          (let ([waiters '()] [result (cons status term-signal)])
            (with-mutex (aio-handle-mutex process)
              (aio-handle-result-set! process result)
              (set! waiters
                (aio-queue-drain! (aio-handle-accept-queue process))))
            (for-each
              (lambda (waiter)
                (unless (aio-waiter-dead? (car waiter))
                  ((cdr waiter) (cons 'values (list status term-signal)))))
              waiters)))))))

(define aio-watch-payload
  (lambda (h kind status aux)
    (cond
      [(fx= kind AIO-EV-SIGNAL) (cons 'values (list status))]
      [(fx< status 0)
       (cons 'raise
         (aio-io-condition (aio-handle-kind h) h (aio-handle-path h) status))]
      [(fx= kind AIO-EV-FS-EVENT)
       (let ([buf (make-bytevector 4097)])
         (let ([events (aio-fs-event-result-copy aux buf 4097)])
           (if (fx< events 0)
               (cons 'raise
                 (aio-io-condition 'fs-event h (aio-handle-path h) events))
               (cons 'values
                 (list (bv->cstring buf)
                       (append (if (fxlogtest events 1) '(rename) '())
                               (if (fxlogtest events 2) '(change) '())))))))]
      [else
       (let ([stat
              (lambda (current?)
                (define (field i)
                  (aio-fs-poll-result-field aux (if current? 1 0) i))
                (list (cons 'dev (field 0)) (cons 'mode (field 1))
                      (cons 'nlink (field 2)) (cons 'uid (field 3))
                      (cons 'gid (field 4)) (cons 'rdev (field 5))
                      (cons 'ino (field 6)) (cons 'size (field 7))
                      (cons 'blksize (field 8)) (cons 'blocks (field 9))
                      (cons 'atime (cons (field 12) (field 13)))
                      (cons 'mtime (cons (field 14) (field 15)))
                      (cons 'ctime (cons (field 16) (field 17)))))])
         (cons 'values (list (stat #f) (stat #t))))])))

(define aio-on-watch
  (lambda (st id kind status aux)
    (let ([h (aio-lookup-handle st id)] [waiter #f])
      (when h
        (with-mutex (aio-handle-mutex h)
          (aio-handle-reading?-set! h #f)
          (set! waiter (aio-queue-pop-live! (aio-handle-read-queue h)))))
      (let ([payload (and h waiter (aio-watch-payload h kind status aux))])
        (when (fx= kind AIO-EV-FS-EVENT) (aio-fs-event-result-free aux))
        (when (fx= kind AIO-EV-FS-POLL) (aio-fs-poll-result-free aux))
        (when (and waiter (not (aio-waiter-dead? (car waiter))))
          ((cdr waiter) payload))))))

;;; attempt one accept; returns a payload, or #f when nothing is pending
(define aio-attempt-accept
  (lambda (st h)
    (let* ([cid (aio-next-id st)]
           [ch ((if (eq? (aio-handle-kind h) 'pipe-listener)
                    aio-pipe-init
                    aio-tcp-init)
                (aio-state-loop st) cid)])
      (if (= ch 0)
          (cons 'raise (aio-io-condition 'accept h (aio-handle-path h) -12))
          (let ([r (aio-accept (aio-handle-handle h) ch)])
            (cond
              [(fx= r 0)
               (let ([w (make-aio-handle cid ch
                          (if (eq? (aio-handle-kind h) 'pipe-listener)
                              'pipe-stream
                              'tcp-stream)
                          st #f #f (make-mutex) #f #f (make-aio-queue) #f #f
                          (make-aio-queue) #f)])
                 (aio-register-handle! st w)
                 (cons 'values (list w)))]
              [(fx= r (aio-eagain-code))
               (aio-handle-close ch)
               #f]
              [else
               (aio-handle-close ch)
               (cons 'raise (aio-io-condition 'accept h (aio-handle-path h) r))]))))))

(define aio-on-accept
  (lambda (st id status)
    (let ([h (aio-lookup-handle st id)])
      (when (and h (not (aio-state-closing? st)))
        (let ([delivery #f])
          (with-mutex (aio-handle-mutex h)
            (let ([waiter (aio-queue-peek-live (aio-handle-accept-queue h))])
              (when waiter
                (let ([payload
                       (if (fx< status 0)
                           (cons 'raise
                             (aio-io-condition 'accept h
                               (aio-handle-path h) status))
                           (aio-attempt-accept st h))])
                  (when payload
                    (aio-queue-pop! (aio-handle-accept-queue h))
                    (set! delivery (cons (cdr waiter) payload)))))))
          (when delivery
            (unless ((car delivery) (cdr delivery))
              (unless (aio-deliver-one-waiter! h
                        (aio-handle-accept-queue h) (cdr delivery))
                ;; Nobody claimed an accepted stream.  Closing it avoids
                ;; retaining a native connection after cancellation won.
                (let ([payload (cdr delivery)])
                  (when (and (eq? (car payload) 'values)
                             (pair? (cdr payload))
                             (aio-handle? (cadr payload)))
                    (aio-close-handle (cadr payload) 'accept)))))))))))

(define aio-on-close
  (lambda (st id)
    (let ([h (aio-lookup-handle st id)])
      (when h
        (with-mutex (aio-handle-mutex h)
          (aio-handle-closed?-set! h #t)))
      (hashtable-delete! (aio-state-handles st) id))))

(define aio-drain-completions!
  (lambda (st)
    (let ([queue (aio-state-completions st)])
      (let loop ([n aio-dispatch-bound])
        (unless (fx= n 0)
          (let ([completion (aio-completion-queue-pop! queue)])
            (when completion
              (aio-dispatch-event st
                (aio-completion-id completion)
                (aio-completion-kind completion)
                (aio-completion-status completion)
                (aio-completion-aux completion))
              (loop (fx- n 1)))))))))

;;; deferred uv_read_stop requests: set by cancellation nacks that may run
;;; off the scheduler thread, drained by the poll hook on the scheduler thread
(define aio-drain-stop-set!
  (lambda (st)
    (let ([hs (with-mutex (aio-state-stop-mutex st)
                (let ([hs (aio-state-stop-set st)])
                  (aio-state-stop-set-set! st '())
                  hs))])
      (for-each
        (lambda (h)
          (with-mutex (aio-handle-mutex h)
            (when (and (aio-handle-reading? h)
                       (aio-queue-empty? (aio-handle-read-queue h)))
              (aio-handle-reading?-set! h #f)
              ((case (aio-handle-kind h)
                 [(udp) aio-udp-recv-stop]
                 [(poll) aio-poll-stop]
                 [(signal) aio-signal-stop]
                 [(fs-event) aio-fs-event-stop]
                 [(fs-poll) aio-fs-poll-stop]
                 [else aio-read-stop])
               (aio-handle-handle h)))))
        hs))))

(define aio-drain-guardian!
  (lambda (st)
    (let ([g (aio-state-guardian st)])
      (let loop ()
        (let ([w (g)])
          (when w
            (aio-close-handle w 'finalized)
            (loop)))))))

(define aio-finalize-file!
  (lambda (f)
    (let ([close?
           (with-mutex (async-file-mutex f)
             (if (async-file-closed? f)
                 #f
                 (begin
                   (async-file-closed?-set! f #t)
                   #t)))])
      (when close?
        (aio-fs-close-now (async-file-fd f)))
      (aio-unregister-file! f))))

(define aio-drain-file-guardian!
  (lambda (st)
    (let ([g (aio-state-file-guardian st)])
      (let loop ()
        (let ([f (g)])
          (when f
            (aio-finalize-file! f)
            (loop)))))))

(define aio-finalize-directory!
  (lambda (d)
    (let ([close?
           (with-mutex (async-directory-mutex d)
             (if (async-directory-closed? d)
                 #f
                 (begin
                   (async-directory-closed?-set! d #t)
                   #t)))])
      (when close? (aio-fs-closedir-now (async-directory-pointer d)))
      (aio-unregister-directory! d))))

(define aio-drain-directory-guardian!
  (lambda (st)
    (let ([g (aio-state-directory-guardian st)])
      (let loop ()
        (let ([d (g)])
          (when d
            (aio-finalize-directory! d)
            (loop)))))))

;;; ------------------------------------------------------- poll and wake

(define AIO-IDLE-RECHECK-MS 100)

(define aio-arm-bridge
  (lambda (st sched)
    (let ([timer ($async-scheduler-timers sched)])
      ;; Cross-thread notifications remain the fast path.  The bounded bridge
      ;; interval guarantees that a coalesced native wakeup cannot leave
      ;; Scheme-side work behind a permanently blocking UV_RUN_ONCE.
      (let ([timeout-ms
             (if timer
                 (let* ([deadline ($async-timer-deadline timer)]
                        [delta (max 0 (- deadline ($async-monotonic-us)))])
                   (min AIO-IDLE-RECHECK-MS
                     (quotient (+ delta 999) 1000)))
                 AIO-IDLE-RECHECK-MS)])
        (aio-bridge-start (aio-state-loop st) timeout-ms)))))

(define aio-poll
  (lambda (sched block?)
    (let ([st ($async-scheduler-io-state sched)])
      (when (and st (not (aio-state-closing? st)))
        (aio-debug-check-owner! st)
        (aio-drain-guardian! st)
        (aio-drain-file-guardian! st)
        (aio-drain-directory-guardian! st)
        (aio-drain-commands! st)
        (aio-drain-stop-set! st)
        (when block? (aio-arm-bridge st sched))
        (aio-loop-run (aio-state-loop st) (if block? 1 0))
        (when block? (aio-bridge-stop (aio-state-loop st)))
        (aio-drain-completions! st)))))

;;; ------------------------------------------------------------ shutdown

;;; Orderly native shutdown (ASYNC.md "Resource finalization"): close every
;;; handle, cancel in-flight requests, run the loop until the close and
;;; cancellation callbacks complete, then release the loop.  Tasks are
;;; already terminal when this runs, so completions only reclaim native
;;; resources.
(define aio-io-shutdown
  (lambda (sched)
    (let ([st ($async-scheduler-io-state sched)])
      (when (and st (not (aio-state-closing? st)))
        (aio-debug-check-owner! st)
        (with-mutex (aio-state-command-mutex st)
          (aio-state-closing?-set! st #t))
        (aio-drain-commands! st)
        (let-values ([(ks vs) (hashtable-entries (aio-state-handles st))])
          (vector-for-each
            (lambda (p)
              (let ([w (car p)])
                (unless (bwp-object? w)
                  (aio-handle-closing?-set! w #t)
                  (unless (fx= 1 (aio-handle-is-closing (aio-handle-handle w)))
                    (aio-handle-close (aio-handle-handle w))))))
            vs))
        (with-mutex (aio-state-requests-mutex st)
          (let-values ([(ks vs) (hashtable-entries (aio-state-requests st))])
            (vector-for-each
              (lambda (req)
                (aio-req-canceled?-set! req #t)
                (let ([cd (aio-req-cancel-data req)])
                  (when cd
                    (case (aio-req-kind req)
                      [(fs) (aio-fs-cancel cd)]
                      [(dns) (aio-dns-cancel cd)]
                      [(nameinfo) (aio-dns-reverse-cancel cd)]
                      [(random) (aio-random-cancel cd)]
                      [else (void)]))))
              vs)))
        (aio-handle-close (aio-state-wakeup st))
        (aio-handle-close (aio-state-bridge st))
        (let loop ()
          (aio-loop-run (aio-state-loop st) 0)
          (aio-drain-completions! st)
          (when (fx= 1 (aio-loop-alive (aio-state-loop st)))
            (aio-loop-run (aio-state-loop st) 1)
            (aio-drain-completions! st)
            (loop)))
        (let drain ()
          (unless (aio-completion-queue-empty? (aio-state-completions st))
            (aio-drain-completions! st)
            (drain)))
        ;; No native request remains at this point, so synchronous close cannot
        ;; race an in-flight read, write, or async close request.
        (let-values ([(fds files) (hashtable-entries (aio-state-files st))])
          (vector-for-each
            (lambda (p)
              (let ([f (car p)])
                (unless (bwp-object? f)
                  (aio-finalize-file! f))))
            files))
        (let-values ([(pointers directories)
                      (hashtable-entries (aio-state-directories st))])
          (vector-for-each
            (lambda (p)
              (let ([d (car p)])
                (unless (bwp-object? d) (aio-finalize-directory! d))))
            directories))
        (aio-invariant (fx= (hashtable-size (aio-state-requests st)) 0)
          "loop shutdown retained native requests" st)
        (aio-invariant (fx= (hashtable-size (aio-state-handles st)) 0)
          "loop shutdown retained native handles" st)
        (aio-invariant (fx= (hashtable-size (aio-state-files st)) 0)
          "loop shutdown retained native files" st)
        (aio-invariant (fx= (hashtable-size (aio-state-directories st)) 0)
          "loop shutdown retained native directories" st)
        (aio-loop-destroy (aio-state-loop st))))))

;;; -------------------------------------------------------- state setup

(define aio-ensure-state!
  (lambda (who)
    (aio-resolve-kernel!)
    (let ([sched (current-async-scheduler)])
      (unless sched
        ($oops who "not in an async scheduler"))
      (when ($async-scheduler-virtual? sched)
        ($oops who "asynchronous I/O is not available on a virtual-clock scheduler"))
      (or ($async-scheduler-io-state sched)
          (let ([loop (aio-loop-open)])
            (when (= loop 0)
              ($oops who "cannot create a libuv loop"))
            (let ([wakeup (aio-wakeup-init loop)])
              (when (= wakeup 0)
                (aio-loop-destroy loop)
                ($oops who "cannot initialize the libuv wakeup handle"))
              (let ([bridge (aio-bridge-init loop)])
                (when (= bridge 0)
                  (aio-handle-close wakeup)
                  (let drain ()
                    (when (fx= 1 (aio-loop-alive loop))
                      (aio-loop-run loop 0)
                      (drain)))
                  (aio-loop-destroy loop)
                  ($oops who "cannot initialize the libuv timer handle"))
                (let ([st (make-aio-state sched loop wakeup bridge
                        1 (make-eq-hashtable) (make-mutex)
                        (make-eq-hashtable) (make-eq-hashtable)
                        (make-eq-hashtable)
                        (make-aio-completion-queue) '() (make-mutex)
                        '() (make-mutex) #f
                        (make-guardian) (make-guardian) (make-guardian))])
                  (aio-set-notify loop (foreign-callable-entry-point aio-notify-trampoline))
                  ($async-scheduler-io-state-set! sched st)
                  ($async-scheduler-poll-proc-set! sched aio-poll)
                  ($async-scheduler-wake-proc-set! sched
                    (lambda ()
                      (with-mutex (aio-state-command-mutex st)
                        (unless (aio-state-closing? st)
                          (aio-wakeup-send loop)))))
                  st))))))))

;;; ------------------------------------------------------------ operations

;;; A libuv wait resumes once on the scheduler that owns its registration.
;;; The scheduler clears the pin when it claims that turn, so the task is
;;; eligible for ordinary work stealing at its next suspension.
(define aio-make-operation
  (case-lambda
    [(try block)
     (aio-make-operation try block (lambda (values) values)
       (lambda (ss) (void)))]
    [(try block wrap)
     (aio-make-operation try block wrap (lambda (ss) (void)))]
    [(try block wrap nack)
     (make-operation try
       (lambda (ss deliver)
         ($async-pin-current-wait!)
         (block ss deliver))
       wrap nack)]))

;;; ------------------------------------------------------------ handles

;;; owner-thread close: wakes pending readers/writers/acceptors with a
;;; closed-handle condition, then closes the native handle
(define aio-cancel-handle-requests!
  (lambda (w operation)
    (let ([deliveries '()])
      (with-mutex (aio-state-requests-mutex (aio-handle-state w))
        (let-values ([(ids reqs)
                      (hashtable-entries
                        (aio-state-requests (aio-handle-state w)))])
          (vector-for-each
            (lambda (req)
              (when (and (eq? (aio-req-handle req) w)
                         (not (aio-req-canceled? req)))
                (let ([deliver (aio-req-deliver req)])
                  (aio-req-canceled?-set! req #t)
                  (aio-req-deliver-set! req #f)
                  (when deliver
                    (set! deliveries (cons deliver deliveries))))))
            reqs)))
      (let ([payload (cons 'raise (aio-closed-condition operation w))])
        (for-each (lambda (deliver) (deliver payload)) deliveries)))))

(define aio-close-handle
  (lambda (w operation)
    (aio-debug-check-owner! (aio-handle-state w))
    (let-values ([(close? waiters)
                  (with-mutex (aio-handle-mutex w)
                    (if (aio-handle-closing? w)
                        (values #f '())
                        (begin
                          (aio-handle-closing?-set! w #t)
                          (let ([waiters
                                 (append
                                   (aio-queue-drain! (aio-handle-read-queue w))
                                   (aio-queue-drain!
                                     (aio-handle-accept-queue w)))])
                            (values #t waiters)))))])
      (when close?
        (let ([payload (cons 'raise (aio-closed-condition operation w))])
          (for-each
            (lambda (waiter)
              (unless (aio-waiter-dead? (car waiter))
                ((cdr waiter) payload)))
            waiters))
        (aio-cancel-handle-requests! w operation)
        (aio-handle-close (aio-handle-handle w))))))

;;; ------------------------------------------------------------- streams

(define aio-check-stream
  (lambda (who s)
    (unless (async-stream? s)
      ($oops who "~s is not an async stream" s))))

(define aio-check-stream-unowned
  (lambda (who s)
    (aio-check-stream who s)
    (with-mutex (aio-handle-mutex s)
      (when (aio-handle-port-owned? s)
        ($oops who "async stream ownership has been transferred to a port")))))

(define aio-check-handle-scope!
  (lambda (who h)
    (let ([sched (current-async-scheduler)])
      (unless (and sched
                   (eq? ($async-scheduler-group-token sched)
                        ($async-scheduler-group-token
                          (aio-state-owner (aio-handle-state h)))))
        ($oops who "async handle belongs to another scheduler group")))))

(define aio-check-stream-access!
  (lambda (who s allow-owned?)
    (aio-check-stream who s)
    (aio-check-handle-scope! who s)
    (unless allow-owned?
      (with-mutex (aio-handle-mutex s)
        (when (aio-handle-port-owned? s)
          ($oops who "async stream ownership has been transferred to a port"))))))

(define aio-claim-stream-for-port!
  (lambda (who s)
    (aio-check-stream who s)
    (aio-check-handle-scope! who s)
    (with-mutex (aio-handle-mutex s)
      (when (aio-handle-port-owned? s)
        ($oops who "async stream ownership has already been transferred to a port"))
      (when (aio-handle-closing? s)
        (raise (aio-closed-condition who s)))
      (aio-handle-port-owned?-set! s #t))))

(define aio-close-owned-handle
  (lambda (who h)
    (aio-check-handle-scope! who h)
    (aio-run-on-owner! (aio-handle-state h)
      (lambda () (aio-close-handle h 'close)))
    (void)))

(define aio-start-stream-read!
  (lambda (s)
    (with-mutex (aio-handle-mutex s)
      (when (and (not (aio-handle-reading? s))
                 (not (aio-handle-closing? s))
                 (not (aio-handle-eof? s))
                 (not (aio-queue-empty? (aio-handle-read-queue s))))
        (aio-handle-reading?-set! s #t)
        (aio-read-start (aio-handle-handle s))))))

(define %stream-read-operation
  (lambda (s . allow-owned-option)
    (aio-check-stream 'stream-read-operation s)
    (let ([token (list 'stream-read-operation)]
          [allow-owned? (and (pair? allow-owned-option)
                             (car allow-owned-option))])
      (aio-make-operation
        (lambda (ss)
          (aio-check-stream-access! 'stream-read-operation s allow-owned?)
        (with-mutex (aio-handle-mutex s)
          (cond
            [(aio-handle-eof? s) (cons 'values (list #!eof))]
            [(aio-handle-closing? s)
             (cons 'raise (aio-closed-condition 'read s))]
            [else #f])))
        (lambda (ss deliver)
          (aio-check-stream-access! 'stream-read-operation s allow-owned?)
          (let ([result
                 (with-mutex (aio-handle-mutex s)
                   (cond
                     [(aio-handle-eof? s)
                      (cons 'immediate (cons 'values (list #!eof)))]
                     [(aio-handle-closing? s)
                      (cons 'immediate
                        (cons 'raise (aio-closed-condition 'read s)))]
                     [else
                      (let ([node
                             (aio-queue-push! (aio-handle-read-queue s)
                               (cons ss deliver))])
                        ($async-sync-slot-set! ss token node)
                        '(blocked))]))])
            (if (eq? (car result) 'blocked)
                (begin
                  (aio-run-on-owner! (aio-handle-state s)
                    (lambda () (aio-start-stream-read! s)))
                  (list 'read (aio-handle-id s)))
                (begin (deliver (cdr result)) #f))))
        (lambda (vals) vals)
        (lambda (ss)
          (let ([node ($async-sync-slot-ref ss token #f)])
            (when node
              ($async-sync-slot-delete! ss token)
              (with-mutex (aio-handle-mutex s)
                (aio-queue-remove! (aio-handle-read-queue s) node))))
          (let ([st (aio-handle-state s)])
            (with-mutex (aio-state-stop-mutex st)
              (aio-state-stop-set-set! st
                (cons s (aio-state-stop-set st))))))))))

(define %stream-write-operation
  (lambda (s bv . allow-owned-option)
    (aio-check-stream 'stream-write-operation s)
    (unless (bytevector? bv)
      ($oops 'stream-write-operation "~s is not a bytevector" bv))
    (let ([token (list 'stream-write-operation)]
          [allow-owned? (and (pair? allow-owned-option)
                             (car allow-owned-option))])
      (aio-make-operation
        (lambda (ss)
          (aio-check-stream-access! 'stream-write-operation s allow-owned?)
          (with-mutex (aio-handle-mutex s)
            (and (aio-handle-closing? s)
                 (cons 'raise (aio-closed-condition 'write s)))))
        (lambda (ss deliver)
          (aio-check-stream-access! 'stream-write-operation s allow-owned?)
          (if (with-mutex (aio-handle-mutex s)
                (aio-handle-closing? s))
              (begin
                (deliver (cons 'raise (aio-closed-condition 'write s)))
                #f)
              (let* ([st (aio-handle-state s)]
                     [id-box (box #f)]
                     [canceled-box (box #f)]
                     [attempt (vector st id-box canceled-box)]
                     [len (bytevector-length bv)])
                ($async-sync-slot-set! ss token attempt)
                (if (aio-run-on-owner! st
                      (lambda ()
                        (cond
                          [(or (aio-atomic-box-ref canceled-box)
                               (aio-waiter-dead? ss))
                           (void)]
                          [(with-mutex (aio-handle-mutex s)
                             (aio-handle-closing? s))
                           (deliver
                             (cons 'raise (aio-closed-condition 'write s)))]
                          [else
                          (let* ([id (aio-next-id st)]
                                 [r (aio-write
                                      (aio-handle-handle s) bv len id)])
                            (aio-atomic-box-set-once! id-box id)
                            (if (< r 0)
                                (deliver
                                  (cons 'raise
                                    (aio-io-condition 'write s
                                      (aio-handle-path s) r)))
                                (begin
                                  (aio-register-request! st id
                                    (make-aio-req 'write s deliver #f
                                      (aio-plain-finish
                                        (lambda (status aux)
                                          (if (fx= status 0)
                                              (cons 'values (list len))
                                              (cons 'raise
                                                (aio-io-condition 'write s
                                                  (aio-handle-path s) status)))))
                                      #f))
                                  (when (or (aio-atomic-box-ref canceled-box)
                                            (aio-waiter-dead? ss))
                                    (aio-cancel-request! st id)))))])))
                    (list 'write (aio-handle-id s))
                    (begin
                      (deliver (cons 'raise (aio-closed-condition 'write s)))
                      #f)))))
        (lambda (vals) vals)
        (aio-request-nack token)))))

(define stream-shutdown-operation
  (lambda (s)
    (aio-check-stream 'stream-shutdown s)
    (let ([token (list 'stream-shutdown-operation)])
      (aio-make-operation
        (lambda (ss)
          (aio-check-stream-access! 'stream-shutdown s #f)
          (with-mutex (aio-handle-mutex s)
            (and (aio-handle-closing? s)
                 (cons 'raise (aio-closed-condition 'shutdown s)))))
        (lambda (ss deliver)
          (aio-check-stream-access! 'stream-shutdown s #f)
          (if (with-mutex (aio-handle-mutex s)
                (aio-handle-closing? s))
              (begin
                (deliver (cons 'raise (aio-closed-condition 'shutdown s)))
                #f)
              (let* ([st (aio-handle-state s)]
                     [id-box (box #f)]
                     [canceled-box (box #f)]
                     [attempt (vector st id-box canceled-box)])
                ($async-sync-slot-set! ss token attempt)
                (if (aio-run-on-owner! st
                      (lambda ()
                        (cond
                          [(or (aio-atomic-box-ref canceled-box)
                               (aio-waiter-dead? ss))
                           (void)]
                          [(with-mutex (aio-handle-mutex s)
                             (aio-handle-closing? s))
                           (deliver
                             (cons 'raise (aio-closed-condition 'shutdown s)))]
                          [else
                          (let* ([id (aio-next-id st)]
                                 [r (aio-shutdown (aio-handle-handle s) id)])
                            (aio-atomic-box-set-once! id-box id)
                            (if (< r 0)
                                (deliver
                                  (cons 'raise
                                    (aio-io-condition 'shutdown s
                                      (aio-handle-path s) r)))
                                (begin
                                  (aio-register-request! st id
                                    (make-aio-req 'shutdown s deliver #f
                                      (aio-plain-finish
                                        (lambda (status aux)
                                          (if (fx= status 0)
                                              (cons 'values '())
                                              (cons 'raise
                                                (aio-io-condition 'shutdown s
                                                  (aio-handle-path s) status)))))
                                      #f))
                                  (when (or (aio-atomic-box-ref canceled-box)
                                            (aio-waiter-dead? ss))
                                    (aio-cancel-request! st id)))))])))
                    (list 'shutdown (aio-handle-id s))
                    (begin
                      (deliver
                        (cons 'raise (aio-closed-condition 'shutdown s)))
                      #f)))))
        (lambda (vals) vals)
        (aio-request-nack token)))))

;;; The request identity belongs to one perform, not to the reusable operation.
(define aio-request-nack
  (lambda (token)
    (lambda (ss)
      (let ([attempt ($async-sync-slot-ref ss token #f)])
        (when attempt
          ($async-sync-slot-delete! ss token)
          (if (vector? attempt)
              (let ([st (vector-ref attempt 0)]
                    [id-box (vector-ref attempt 1)])
                (aio-atomic-box-flag! (vector-ref attempt 2))
                (let ([id (aio-atomic-box-ref id-box)])
                  (when id (aio-cancel-request! st id))))
              (aio-cancel-request! (car attempt) (cdr attempt))))))))

;;; ------------------------------------------------------------- tcp

(define aio-check-host-port
  (lambda (who host port)
    (unless (string? host) ($oops who "~s is not a string" host))
    (unless (and (fixnum? port) (fx>= port 0) (fx<= port 65535))
      ($oops who "~s is not a valid port number" port))))

(define %tcp-listen
  (case-lambda
    [(host port) (%tcp-listen host port 128)]
    [(host port backlog)
     (aio-check-host-port 'tcp-listen host port)
     (unless (and (fixnum? backlog) (fx> backlog 0))
       ($oops 'tcp-listen "~s is not a positive fixnum" backlog))
     (let* ([st (aio-ensure-state! 'tcp-listen)]
            [id (aio-next-id st)]
            [h (aio-tcp-init (aio-state-loop st) id)])
       (when (= h 0)
         ($oops 'tcp-listen "cannot allocate a tcp handle"))
       (let ([w (make-aio-handle id h 'tcp-listener st
                  (format "~a:~a" host port) #f (make-mutex)
                  #f #f (make-aio-queue) #f #f (make-aio-queue) #f)])
         (define (fail r)
           (aio-handle-close h)
           (raise (aio-io-condition 'listen w (aio-handle-path w) r)))
         (let ([r (aio-tcp-bind h host port)])
           (when (fx< r 0) (fail r)))
         (let ([r (aio-listen-start h backlog)])
           (when (fx< r 0) (fail r)))
         (aio-register-handle! st w)
         w))]))

(define %tcp-accept-operation
  (lambda (listener)
    (unless (tcp-listener? listener)
      ($oops 'tcp-accept-operation "~s is not a tcp listener" listener))
    (let ([st (aio-handle-state listener)]
          [token (list 'tcp-accept-operation)])
      (aio-make-operation
        (lambda (ss)
          (aio-check-handle-scope! 'tcp-accept-operation listener)
          (with-mutex (aio-handle-mutex listener)
            (cond
              [(aio-handle-closing? listener)
               (cons 'raise (aio-closed-condition 'accept listener))]
              [else #f])))
        (lambda (ss deliver)
          (aio-check-handle-scope! 'tcp-accept-operation listener)
          (let ([payload
                 (with-mutex (aio-handle-mutex listener)
                   (if (aio-handle-closing? listener)
                       (cons 'raise (aio-closed-condition 'accept listener))
                       (begin
                         ($async-sync-slot-set! ss token
                           (aio-queue-push!
                             (aio-handle-accept-queue listener)
                             (cons ss deliver)))
                         #f)))])
            (if (not payload)
                (begin
                  (aio-run-on-owner! st
                    (lambda ()
                      ;; A connection may already be pending before libuv
                      ;; reports another listener event.
                      (aio-on-accept st (aio-handle-id listener) 0)))
                  (list 'accept (aio-handle-id listener)))
                (begin (deliver payload) #f))))
        (lambda (vals) vals)
        (lambda (ss)
          (let ([node ($async-sync-slot-ref ss token #f)])
            (when node
              ($async-sync-slot-delete! ss token)
              (with-mutex (aio-handle-mutex listener)
                (aio-queue-remove!
                  (aio-handle-accept-queue listener) node)))))))))

(define %tcp-connect-operation
  (lambda (host port)
    (aio-check-host-port 'tcp-connect-operation host port)
    (let ([token (list 'tcp-connect-operation)])
      (aio-make-operation
        (lambda (ss) #f)
        (lambda (ss deliver)
          (let* ([st (aio-ensure-state! 'tcp-connect-operation)]
                 [id (aio-next-id st)]
                 [h (aio-tcp-init (aio-state-loop st) id)])
            ($async-sync-slot-set! ss token (cons st id))
            (if (= h 0)
                (begin
                  (deliver
                    (cons 'raise
                      (aio-io-condition 'connect #f (format "~a:~a" host port) -12)))
                  #f)
                (let ([w (make-aio-handle id h 'tcp-stream st
                           (format "~a:~a" host port) #f (make-mutex)
                           #f #f (make-aio-queue) #f #f (make-aio-queue) #f)])
                  (aio-register-handle! st w)
                  (let ([r (aio-tcp-connect h host port id)])
                    (if (< r 0)
                        (begin
                          (aio-close-handle w 'connect)
                          (deliver
                            (cons 'raise
                              (aio-io-condition 'connect w (aio-handle-path w) r)))
                          #f)
                        (begin
                          (aio-register-request! st id
                            (make-aio-req 'connect w deliver #f
                              (lambda (canceled? status aux)
                                (cond
                                  [canceled?
                                   (aio-close-handle w 'connect)
                                   #f]
                                  [(fx= status 0)
                                   (cons 'values (list w))]
                                  [else
                                   (aio-close-handle w 'connect)
                                   (cons 'raise
                                     (aio-io-condition 'connect w
                                       (aio-handle-path w) status))]))
                              #f))
                          (list 'connect id))))))))
        (lambda (vals) vals)
        ;; a connect cannot be canceled in libuv; the completion closes the
        ;; handle and is dropped because the request is marked canceled
        (aio-request-nack token)))))

;;; ------------------------------------------------------ local-domain

(define %pipe-listen
  (case-lambda
    [(path) (%pipe-listen path 128)]
    [(path backlog)
     (unless (string? path) ($oops 'pipe-listen "~s is not a string" path))
     (unless (and (fixnum? backlog) (fx> backlog 0))
       ($oops 'pipe-listen "~s is not a positive fixnum" backlog))
     (let* ([st (aio-ensure-state! 'pipe-listen)]
            [id (aio-next-id st)]
            [h (aio-pipe-init (aio-state-loop st) id)])
       (when (= h 0)
         ($oops 'pipe-listen "cannot allocate a pipe handle"))
       (let ([w (make-aio-handle id h 'pipe-listener st path #f (make-mutex)
                  #f #f (make-aio-queue) #f #f (make-aio-queue) #f)])
         (define (fail r)
           (aio-handle-close h)
           (raise (aio-io-condition 'listen w path r)))
         (let ([r (aio-pipe-bind h path)])
           (when (fx< r 0) (fail r)))
         (let ([r (aio-listen-start h backlog)])
           (when (fx< r 0) (fail r)))
         (aio-register-handle! st w)
         w))]))

(define %pipe-connect-operation
  (lambda (path)
    (unless (string? path)
      ($oops 'pipe-connect-operation "~s is not a string" path))
    (let ([token (list 'pipe-connect-operation)])
      (aio-make-operation
        (lambda (ss) #f)
        (lambda (ss deliver)
          (let* ([st (aio-ensure-state! 'pipe-connect-operation)]
                 [id (aio-next-id st)]
                 [h (aio-pipe-init (aio-state-loop st) id)])
            ($async-sync-slot-set! ss token (cons st id))
            (if (= h 0)
                (begin
                  (deliver
                    (cons 'raise (aio-io-condition 'connect #f path -12)))
                  #f)
                (let ([w (make-aio-handle id h 'pipe-stream st path #f (make-mutex)
                           #f #f (make-aio-queue) #f #f (make-aio-queue) #f)])
                  (aio-register-handle! st w)
                  (aio-register-request! st id
                    (make-aio-req 'connect w deliver #f
                      (lambda (canceled? status aux)
                        (cond
                          [canceled?
                           (aio-close-handle w 'connect)
                           #f]
                          [(fx= status 0)
                           (cons 'values (list w))]
                          [else
                           (aio-close-handle w 'connect)
                           (cons 'raise
                             (aio-io-condition 'connect w path status))]))
                      #f))
                  (aio-pipe-connect h path id)
                  (list 'connect id)))))
        (lambda (vals) vals)
        (aio-request-nack token)))))

;;; ---------------------------------------------------------------- dns

(define %dns-lookup-operation
  (case-lambda
    [(node) (%dns-lookup-operation node #f)]
    [(node service)
     (unless (string? node)
       ($oops 'dns-lookup-operation "~s is not a string" node))
     (unless (or (not service) (string? service))
       ($oops 'dns-lookup-operation "~s is not a string or #f" service))
     (let ([token (list 'dns-lookup-operation)])
       (aio-make-operation
         (lambda (ss) #f)
         (lambda (ss deliver)
           (let* ([st (aio-ensure-state! 'dns-lookup-operation)]
                  [id (aio-next-id st)]
                  [r (aio-dns-lookup (aio-state-loop st) node
                       (or service "") id)])
             ($async-sync-slot-set! ss token (cons st id))
             (if (< r 0)
                 (begin
                   (deliver
                     (cons 'raise (aio-io-condition 'dns #f node r)))
                   #f)
                 (begin
                   (aio-register-request! st id
                     (make-aio-req 'dns #f deliver r
                       (aio-dns-finish
                         (lambda (status aux)
                           (if (fx= status 0)
                               (let ([n (aio-dns-count aux)])
                                 (let loop ([i 0] [acc '()])
                                   (if (fx= i n)
                                       (cons 'values (list (reverse acc)))
                                       (let ([buf (make-bytevector 64)])
                                         (let ([fp (aio-dns-addr aux i buf 64)])
                                           (if (< fp 0)
                                               (loop (fx+ i 1) acc)
                                               (loop (fx+ i 1)
                                                     (cons (list (bv->cstring buf)
                                                             (fxmod fp 65536)
                                                             (quotient fp 65536))
                                                           acc))))))))
                               (cons 'raise (aio-io-condition 'dns #f node status)))))
                       #f))
                   (list 'dns id)))))
         (lambda (vals) vals)
         (aio-request-nack token)))]))

;;; ------------------------------------------------------------------ udp

(define aio-check-udp
  (lambda (who socket)
    (unless (and (aio-handle? socket)
                 (eq? (aio-handle-kind socket) 'udp))
      ($oops who "~s is not a UDP socket" socket))))

(define aio-check-udp-open!
  (lambda (who socket)
    (aio-check-udp who socket)
    (aio-check-handle-scope! who socket)
    (when (with-mutex (aio-handle-mutex socket)
            (aio-handle-closing? socket))
      (raise (aio-closed-condition who socket)))))

(define aio-udp-bind-flag-bits
  (lambda (who flags)
    (unless (and (list? flags) (for-all symbol? flags))
      ($oops who "~s is not a list of UDP bind flags" flags))
    (fold-left
      (lambda (bits flag)
        (case flag
          [(ipv6-only) (fxlogior bits 1)]
          [(reuse-address) (fxlogior bits 2)]
          [else ($oops who "~s is not a UDP bind flag" flag)]))
      0 flags)))

(define %udp-open
  (case-lambda
    [()
     (let* ([st (aio-ensure-state! 'udp-open)]
            [id (aio-next-id st)]
            [h (aio-udp-init (aio-state-loop st) id)])
       (when (= h 0) ($oops 'udp-open "cannot allocate a UDP handle"))
       (let ([socket
              (make-aio-handle id h 'udp st #f #f (make-mutex)
                #f #f (make-aio-queue) #f #f (make-aio-queue) #f)])
         (aio-register-handle! st socket)
         socket))]
    [(host port) (%udp-open host port '())]
    [(host port flags)
     (aio-check-host-port 'udp-open host port)
     (let ([socket (%udp-open)])
       (let ([r (aio-udp-bind (aio-handle-handle socket) host port
                  (aio-udp-bind-flag-bits 'udp-open flags))])
         (if (fx< r 0)
             (begin
               (aio-close-handle socket 'udp-open)
               (raise (aio-io-condition 'udp-bind socket
                        (format "~a:~a" host port) r)))
             socket)))]))

(define aio-start-udp-recv!
  (lambda (socket)
    (let ([delivery #f])
      (with-mutex (aio-handle-mutex socket)
        (when (and (not (aio-handle-reading? socket))
                   (not (aio-handle-closing? socket))
                   (not (aio-queue-empty? (aio-handle-read-queue socket))))
          (aio-handle-reading?-set! socket #t)
          (let ([r (aio-udp-recv-start (aio-handle-handle socket))])
            (when (fx< r 0)
              (aio-handle-reading?-set! socket #f)
              (let ([waiter
                     (aio-queue-pop-live! (aio-handle-read-queue socket))])
                (when waiter
                  (set! delivery
                    (cons (cdr waiter)
                      (cons 'raise
                        (aio-io-condition 'udp-receive socket
                          (aio-handle-path socket) r))))))))))
      (when delivery ((car delivery) (cdr delivery))))))

(define %udp-receive-operation
  (lambda (socket)
    (aio-check-udp 'udp-receive-operation socket)
    (let ([token (list 'udp-receive-operation)])
      (aio-make-operation
      (lambda (ss)
        (aio-check-udp-open! 'udp-receive-operation socket)
        #f)
      (lambda (ss deliver)
        (aio-check-udp-open! 'udp-receive-operation socket)
        (let ([payload
               (with-mutex (aio-handle-mutex socket)
                 (if (aio-handle-closing? socket)
                     (cons 'raise
                       (aio-closed-condition 'udp-receive socket))
                     (begin
                       ($async-sync-slot-set! ss token
                         (aio-queue-push! (aio-handle-read-queue socket)
                           (cons ss deliver)))
                       #f)))])
          (when (not payload)
            (aio-run-on-owner! (aio-handle-state socket)
              (lambda () (aio-start-udp-recv! socket))))
          (if payload
              (begin (deliver payload) #f)
              (list 'udp-receive (aio-handle-id socket)))))
      (lambda (vals) vals)
      (lambda (ss)
        (let ([node ($async-sync-slot-ref ss token #f)])
          (when node
            ($async-sync-slot-delete! ss token)
            (with-mutex (aio-handle-mutex socket)
              (aio-queue-remove! (aio-handle-read-queue socket) node))))
        (let ([st (aio-handle-state socket)])
          (with-mutex (aio-state-stop-mutex st)
            (aio-state-stop-set-set! st
              (cons socket (aio-state-stop-set st))))))))))

(define %udp-send-operation
  (case-lambda
    [(socket bv) (%udp-send-operation socket bv "" 0)]
    [(socket bv host port)
     (aio-check-udp 'udp-send-operation socket)
     (unless (bytevector? bv)
       ($oops 'udp-send-operation "~s is not a bytevector" bv))
     (unless (string=? host "")
       (aio-check-host-port 'udp-send-operation host port))
     (let ([token (list 'udp-send-operation)])
       (aio-make-operation
         (lambda (ss)
           (aio-check-udp-open! 'udp-send-operation socket)
           #f)
         (lambda (ss deliver)
           (aio-check-udp-open! 'udp-send-operation socket)
           (let* ([st (aio-handle-state socket)]
                  [id-box (box #f)]
                  [canceled-box (box #f)]
                  [len (bytevector-length bv)])
             ($async-sync-slot-set! ss token
               (vector st id-box canceled-box))
             (if (aio-run-on-owner! st
                   (lambda ()
                     (unless (or (aio-atomic-box-ref canceled-box)
                                 (aio-waiter-dead? ss))
                       (let* ([id (aio-next-id st)]
                              [r (aio-udp-send (aio-handle-handle socket)
                                   bv len host port id)])
                         (aio-atomic-box-set-once! id-box id)
                         (if (fx< r 0)
                             (deliver (cons 'raise
                                        (aio-io-condition 'udp-send socket
                                          (aio-handle-path socket) r)))
                             (begin
                               (aio-register-request! st id
                                 (make-aio-req 'udp-send socket deliver #f
                                   (aio-plain-finish
                                     (lambda (status aux)
                                       (if (fx= status 0)
                                           (cons 'values (list len))
                                           (cons 'raise
                                             (aio-io-condition 'udp-send socket
                                               (aio-handle-path socket)
                                               status)))))
                                   #f))
                               (when (or (aio-atomic-box-ref canceled-box)
                                         (aio-waiter-dead? ss))
                                 (aio-cancel-request! st id))))))))
                 (list 'udp-send (aio-handle-id socket))
                 (begin
                   (deliver (cons 'raise
                              (aio-closed-condition 'udp-send socket)))
                   #f))))
         (lambda (vals) vals)
         (aio-request-nack token)))]))

(define aio-udp-control
  (lambda (who socket thunk)
    (aio-check-udp-open! who socket)
    (let ([r (thunk)])
      (when (fx< r 0)
        (raise (aio-io-condition who socket (aio-handle-path socket) r))))))

(define aio-udp-address-list
  (lambda (who socket peer?)
    (aio-check-udp-open! who socket)
    (let ([buf (make-bytevector 64)])
      (let ([encoded (aio-udp-address (aio-handle-handle socket)
                       (if peer? 1 0) buf 64)])
        (if (fx< encoded 0)
            (raise (aio-io-condition who socket (aio-handle-path socket)
                     encoded))
            (let-values ([(host port family)
                          (aio-udp-address-values encoded buf)])
              (list host port family)))))))

;;; ----------------------------------------- reverse DNS, random, fd poll

(define aio-nameinfo-flag-bits
  (lambda (who flags)
    (unless (and (list? flags) (for-all symbol? flags))
      ($oops who "~s is not a list of reverse-DNS flags" flags))
    (fold-left
      (lambda (bits flag)
        (case flag
          [(name-required) (fxlogior bits 1)]
          [(numeric-host) (fxlogior bits 2)]
          [(numeric-service) (fxlogior bits 4)]
          [else ($oops who "~s is not a reverse-DNS flag" flag)]))
      0 flags)))

(define %dns-reverse-operation
  (case-lambda
    [(host port) (%dns-reverse-operation host port '())]
    [(host port flags)
     (aio-check-host-port 'dns-reverse-operation host port)
     (let ([bits (aio-nameinfo-flag-bits 'dns-reverse-operation flags)]
           [token (list 'dns-reverse-operation)])
       (aio-make-operation
         (lambda (ss) #f)
         (lambda (ss deliver)
           (let* ([st (aio-ensure-state! 'dns-reverse-operation)]
                  [id (aio-next-id st)]
                  [ctx (aio-dns-reverse (aio-state-loop st) host port bits id)])
             (if (fx< ctx 0)
                 (begin
                   (deliver
                     (cons 'raise
                       (aio-io-condition 'reverse-dns #f host ctx)))
                   #f)
                 (begin
                   ($async-sync-slot-set! ss token (cons st id))
                   (aio-register-request! st id
                     (make-aio-req 'nameinfo #f deliver ctx
                       (lambda (canceled? status aux)
                         (dynamic-wind
                           (lambda () (void))
                           (lambda ()
                             (and (not canceled?)
                                  (if (fx= status 0)
                                      (let ([host-buf (make-bytevector 1024)]
                                            [service-buf (make-bytevector 256)])
                                        (let ([hr (aio-dns-reverse-copy aux 0
                                                    host-buf 1024)]
                                              [sr (aio-dns-reverse-copy aux 1
                                                    service-buf 256)])
                                          (if (or (fx< hr 0) (fx< sr 0))
                                              (cons 'raise
                                                (aio-io-condition 'reverse-dns
                                                  #f host
                                                  (if (fx< hr 0) hr sr)))
                                              (cons 'values
                                                (list (bv->cstring host-buf)
                                                      (bv->cstring service-buf))))))
                                      (cons 'raise
                                        (aio-io-condition 'reverse-dns #f host
                                          status)))))
                           (lambda () (aio-dns-reverse-free aux))))
                       #f))
                   (list 'reverse-dns id)))))
         (lambda (vals) vals)
         (aio-request-nack token)))]))

(define %random-bytevector-operation
  (lambda (length)
    (unless (and (fixnum? length) (fx>= length 0))
      ($oops 'random-bytevector-operation
        "~s is not a nonnegative fixnum" length))
    (let ([token (list 'random-bytevector-operation)])
      (aio-make-operation
        (lambda (ss) #f)
        (lambda (ss deliver)
          (let* ([st (aio-ensure-state! 'random-bytevector-operation)]
                 [id (aio-next-id st)]
                 [ctx (aio-random (aio-state-loop st) length id)])
            (if (fx< ctx 0)
                (begin
                  (deliver
                    (cons 'raise (aio-io-condition 'random #f #f ctx)))
                  #f)
                (begin
                  ($async-sync-slot-set! ss token (cons st id))
                  (aio-register-request! st id
                    (make-aio-req 'random #f deliver ctx
                      (lambda (canceled? status aux)
                        (dynamic-wind
                          (lambda () (void))
                          (lambda ()
                            (and (not canceled?)
                                 (if (fx= status 0)
                                     (let ([bv (make-bytevector length)])
                                       (aio-random-copy aux bv)
                                       (cons 'values (list bv)))
                                     (cons 'raise
                                       (aio-io-condition 'random #f #f status)))))
                          (lambda () (aio-random-free aux))))
                      #f))
                  (list 'random id))))
        (lambda (vals) vals)
        (aio-request-nack token))))))

(define aio-poll-event-bits
  (lambda (who events)
    (unless (and (list? events) (for-all symbol? events))
      ($oops who "~s is not a list of poll events" events))
    (let ([bits
           (fold-left
             (lambda (bits event)
               (case event
                 [(readable) (fxlogior bits 1)]
                 [(writable) (fxlogior bits 2)]
                 [(disconnect) (fxlogior bits 4)]
                 [(prioritized) (fxlogior bits 8)]
                 [else ($oops who "~s is not a poll event" event)]))
             0 events)])
      (when (fx= bits 0) ($oops who "poll event list is empty"))
      bits)))

(define %fd-poll-open
  (lambda (fd)
    (unless (and (fixnum? fd) (fx>= fd 0))
      ($oops 'fd-poll-open "~s is not a file descriptor" fd))
    (let* ([st (aio-ensure-state! 'fd-poll-open)]
           [id (aio-next-id st)]
           [h (aio-poll-init (aio-state-loop st) fd id)])
      (when (= h 0)
        (raise (aio-io-condition 'fd-poll-open #f #f 'init-failed)))
      (let ([poll (make-aio-handle id h 'poll st (format "fd ~a" fd)
                    #f (make-mutex) #f #f (make-aio-queue) #f #f
                    (make-aio-queue) #f)])
        (aio-register-handle! st poll)
        poll))))

(define %fd-poll-operation
  (lambda (poll events)
    (unless (and (aio-handle? poll) (eq? (aio-handle-kind poll) 'poll))
      ($oops 'fd-poll-operation "~s is not an fd poll handle" poll))
    (let ([bits (aio-poll-event-bits 'fd-poll-operation events)]
          [token (list 'fd-poll-operation)])
      (aio-make-operation
        (lambda (ss)
          (aio-check-handle-scope! 'fd-poll-operation poll)
          (with-mutex (aio-handle-mutex poll)
            (cond
              [(aio-handle-closing? poll)
               (cons 'raise (aio-closed-condition 'fd-poll poll))]
              [(not (aio-queue-empty? (aio-handle-read-queue poll)))
               (cons 'raise
                 (aio-io-condition 'fd-poll poll (aio-handle-path poll)
                   'busy))]
              [else #f])))
        (lambda (ss deliver)
          (aio-check-handle-scope! 'fd-poll-operation poll)
          (let ([result
                 (with-mutex (aio-handle-mutex poll)
                   (cond
                     [(aio-handle-closing? poll)
                      (cons 'immediate
                        (cons 'raise (aio-closed-condition 'fd-poll poll)))]
                     [(not (aio-queue-empty? (aio-handle-read-queue poll)))
                      (cons 'immediate
                        (cons 'raise
                          (aio-io-condition 'fd-poll poll
                            (aio-handle-path poll) 'busy)))]
                     [else
                      ($async-sync-slot-set! ss token
                        (aio-queue-push! (aio-handle-read-queue poll)
                          (cons ss deliver)))
                      (aio-handle-reading?-set! poll #t)
                      '(start)]))])
            (when (eq? (car result) 'start)
              (aio-run-on-owner! (aio-handle-state poll)
                (lambda ()
                  (let ([r (aio-poll-start (aio-handle-handle poll) bits)])
                    (when (fx< r 0)
                      (aio-on-poll (aio-handle-state poll)
                        (aio-handle-id poll) r 0))))))
            (if (eq? (car result) 'start)
                (list 'fd-poll (aio-handle-id poll))
                (begin (deliver (cdr result)) #f))))
        (lambda (vals) vals)
        (lambda (ss)
          (let ([node ($async-sync-slot-ref ss token #f)])
            (when node
              ($async-sync-slot-delete! ss token)
              (with-mutex (aio-handle-mutex poll)
                (aio-queue-remove! (aio-handle-read-queue poll) node))))
          (let ([st (aio-handle-state poll)])
            (with-mutex (aio-state-stop-mutex st)
              (aio-state-stop-set-set! st
                (cons poll (aio-state-stop-set st))))))))))

;;; ---------------------------------------------------------- processes

(define aio-string-has-nul?
  (lambda (s)
    (let loop ([i 0])
      (and (fx< i (string-length s))
           (or (char=? (string-ref s i) #\nul) (loop (fx+ i 1)))))))

(define aio-string-list-blob
  (lambda (who strings empty-ok?)
    (unless (and (list? strings) (for-all string? strings))
      ($oops who "~s is not a list of strings" strings))
    (when (and (null? strings) (not empty-ok?))
      ($oops who "argument list is empty"))
    (for-each
      (lambda (s)
        (when (aio-string-has-nul? s)
          ($oops who "string contains a nul character: ~s" s)))
      strings)
    (if (null? strings)
        (make-bytevector 1 0)
        (let* ([parts (map string->utf8 strings)]
               [length (fold-left
                         (lambda (n bv) (fx+ n (fx+ (bytevector-length bv) 1)))
                         0 parts)]
               [blob (make-bytevector length 0)])
          (let loop ([parts parts] [offset 0])
            (if (null? parts)
                blob
                (let ([n (bytevector-length (car parts))])
                  (bytevector-copy! (car parts) 0 blob offset n)
                  (loop (cdr parts) (fx+ offset (fx+ n 1))))))))))

(define aio-process-option
  (lambda (options key default)
    (let ([entry (assq key options)]) (if entry (cdr entry) default))))

(define aio-process-flags
  (lambda (who flags)
    (unless (and (list? flags) (for-all symbol? flags))
      ($oops who "~s is not a list of process flags" flags))
    (fold-left
      (lambda (bits flag)
        (case flag
          [(detached) (fxlogior bits 1)]
          [(windows-hide) (fxlogior bits 2)]
          [(windows-verbatim-arguments) (fxlogior bits 4)]
          [else ($oops who "~s is not a process flag" flag)]))
      0 flags)))

(define aio-process-stdio
  (lambda (who value child-reads? inherit-fd)
    (cond
      [(eq? value 'pipe) (values (if child-reads? 2 3) -1 #t)]
      [(eq? value 'ignore) (values 0 -1 #f)]
      [(eq? value 'inherit) (values 1 inherit-fd #f)]
      [(and (fixnum? value) (fx>= value 0)) (values 1 value #f)]
      [else ($oops who "~s is not a stdio specification" value)])))

(define %process-spawn
  (case-lambda
    [(file arguments) (%process-spawn file arguments '())]
    [(file arguments options)
     (aio-check-path 'process-spawn file)
     (unless (list? options) ($oops 'process-spawn "~s is not an alist" options))
     (let* ([argv (aio-string-list-blob 'process-spawn
                    (cons file arguments) #f)]
            [environment (aio-process-option options 'environment #f)]
            [env-present? (not (eq? environment #f))]
            [env-strings
             (and env-present?
                  (map (lambda (entry)
                         (unless (and (pair? entry) (string? (car entry))
                                      (string? (cdr entry)))
                           ($oops 'process-spawn
                             "~s is not an environment entry" entry))
                         (string-append (car entry) "=" (cdr entry)))
                       environment))]
            [env (aio-string-list-blob 'process-spawn
                   (or env-strings '()) #t)]
            [cwd (aio-process-option options 'cwd "")]
            [flags (aio-process-flags 'process-spawn
                     (aio-process-option options 'flags '()))]
            [st (aio-ensure-state! 'process-spawn)]
            [process-id (aio-next-id st)])
       (unless (string? cwd) ($oops 'process-spawn "~s is not a cwd" cwd))
       (let-values ([(in-mode in-fd in-pipe?)
                     (aio-process-stdio 'process-spawn
                       (aio-process-option options 'stdin 'pipe) #t 0)]
                    [(out-mode out-fd out-pipe?)
                     (aio-process-stdio 'process-spawn
                       (aio-process-option options 'stdout 'pipe) #f 1)]
                    [(err-mode err-fd err-pipe?)
                     (aio-process-stdio 'process-spawn
                       (aio-process-option options 'stderr 'pipe) #f 2)])
         (let* ([in-id (and in-pipe? (aio-next-id st))]
                [out-id (and out-pipe? (aio-next-id st))]
                [err-id (and err-pipe? (aio-next-id st))]
                [in-h (if in-pipe? (aio-pipe-init (aio-state-loop st) in-id) 0)]
                [out-h (if out-pipe? (aio-pipe-init (aio-state-loop st) out-id) 0)]
                [err-h (if err-pipe? (aio-pipe-init (aio-state-loop st) err-id) 0)])
           (define (close-pipes)
             (when (and in-pipe? (not (= in-h 0))) (aio-handle-close in-h))
             (when (and out-pipe? (not (= out-h 0))) (aio-handle-close out-h))
             (when (and err-pipe? (not (= err-h 0))) (aio-handle-close err-h)))
           (when (or (and in-pipe? (= in-h 0))
                     (and out-pipe? (= out-h 0))
                     (and err-pipe? (= err-h 0)))
             (close-pipes)
             ($oops 'process-spawn "cannot allocate a stdio pipe"))
           (let ([ph (aio-process-spawn (aio-state-loop st) process-id file
                       argv (bytevector-length argv)
                       env (bytevector-length env) (if env-present? 1 0)
                       cwd flags
                       in-mode in-fd in-h out-mode out-fd out-h
                       err-mode err-fd err-h)])
             (if (fx< ph 0)
                 (begin
                   (close-pipes)
                   (raise (aio-io-condition 'process-spawn #f file ph)))
                 (let ([process (make-aio-handle process-id ph 'process st file
                                  #f (make-mutex) #f #f (make-aio-queue) #f #f
                                  (make-aio-queue) #f)]
                       [stdin (and in-pipe?
                                (make-aio-handle in-id in-h 'pipe-stream st
                                  "process stdin" #f (make-mutex)
                                  #f #f (make-aio-queue) #f #f
                                  (make-aio-queue) #f))]
                       [stdout (and out-pipe?
                                 (make-aio-handle out-id out-h 'pipe-stream st
                                   "process stdout" #f (make-mutex)
                                   #f #f (make-aio-queue) #f #f
                                   (make-aio-queue) #f))]
                       [stderr (and err-pipe?
                                 (make-aio-handle err-id err-h 'pipe-stream st
                                   "process stderr" #f (make-mutex)
                                   #f #f (make-aio-queue) #f #f
                                   (make-aio-queue) #f))])
                   (aio-register-handle! st process)
                   (when stdin (aio-register-handle! st stdin))
                   (when stdout (aio-register-handle! st stdout))
                   (when stderr (aio-register-handle! st stderr))
                   (values process stdin stdout stderr)))))))]))

(define %process-wait-operation
  (lambda (process)
    (unless (and (aio-handle? process)
                 (eq? (aio-handle-kind process) 'process))
      ($oops 'process-wait-operation "~s is not an async process" process))
    (let ([token (list 'process-wait-operation)])
      (aio-make-operation
      (lambda (ss)
        (aio-check-handle-scope! 'process-wait-operation process)
        (with-mutex (aio-handle-mutex process)
          (let ([result (aio-handle-result process)])
            (cond
              [result (cons 'values (list (car result) (cdr result)))]
              [(aio-handle-closing? process)
               (cons 'raise (aio-closed-condition 'process-wait process))]
              [else #f]))))
      (lambda (ss deliver)
        (aio-check-handle-scope! 'process-wait-operation process)
        (let ([payload
               (with-mutex (aio-handle-mutex process)
                 (let ([result (aio-handle-result process)])
                   (cond
                     [result
                      (cons 'values (list (car result) (cdr result)))]
                     [(aio-handle-closing? process)
                      (cons 'raise
                        (aio-closed-condition 'process-wait process))]
                     [else
                      ($async-sync-slot-set! ss token
                        (aio-queue-push! (aio-handle-accept-queue process)
                          (cons ss deliver)))
                      #f])))])
          (if payload
              (begin (deliver payload) #f)
              (list 'process-wait (aio-handle-id process)))))
      (lambda (vals) vals)
      (lambda (ss)
        (let ([node ($async-sync-slot-ref ss token #f)])
          (when node
            ($async-sync-slot-delete! ss token)
            (with-mutex (aio-handle-mutex process)
              (aio-queue-remove!
                (aio-handle-accept-queue process) node)))))))))

;;; --------------------------------------- signal and filesystem watchers

(define aio-watch-open
  (lambda (who kind path init config)
    (let* ([st (aio-ensure-state! who)]
           [id (aio-next-id st)]
           [native (init (aio-state-loop st) id)])
      (when (= native 0) ($oops who "cannot allocate a native watcher"))
      (let ([watcher (make-aio-handle id native kind st path #f
                       (make-mutex) #f #f (make-aio-queue) #f #f
                       (make-aio-queue) config)])
        (aio-register-handle! st watcher)
        watcher))))

(define aio-check-watcher
  (lambda (who watcher kind)
    (unless (and (aio-handle? watcher) (eq? (aio-handle-kind watcher) kind))
      ($oops who "~s is not an async ~a watcher" watcher kind))))

(define aio-watch-operation
  (lambda (who watcher kind start)
    (aio-check-watcher who watcher kind)
    (let ([token (list 'watch-operation)])
      (aio-make-operation
      (lambda (ss)
        (aio-check-handle-scope! who watcher)
        (with-mutex (aio-handle-mutex watcher)
          (cond
            [(aio-handle-closing? watcher)
             (cons 'raise (aio-closed-condition who watcher))]
            [(not (aio-queue-empty? (aio-handle-read-queue watcher)))
             (cons 'raise
               (aio-io-condition who watcher (aio-handle-path watcher) 'busy))]
            [else #f])))
      (lambda (ss deliver)
        (aio-check-handle-scope! who watcher)
        (let ([result
               (with-mutex (aio-handle-mutex watcher)
                 (cond
                   [(aio-handle-closing? watcher)
                    (cons 'immediate
                      (cons 'raise (aio-closed-condition who watcher)))]
                   [(not (aio-queue-empty? (aio-handle-read-queue watcher)))
                    (cons 'immediate
                      (cons 'raise
                        (aio-io-condition who watcher
                          (aio-handle-path watcher) 'busy)))]
                   [else
                    ($async-sync-slot-set! ss token
                      (aio-queue-push! (aio-handle-read-queue watcher)
                        (cons ss deliver)))
                    (aio-handle-reading?-set! watcher #t)
                    '(start)]))])
          (when (eq? (car result) 'start)
            (aio-run-on-owner! (aio-handle-state watcher)
              (lambda ()
                (let ([r (start)])
                  (when (fx< r 0)
                    (aio-on-watch (aio-handle-state watcher)
                      (aio-handle-id watcher)
                      (case kind
                        [(signal) AIO-EV-SIGNAL]
                        [(fs-event) AIO-EV-FS-EVENT]
                        [else AIO-EV-FS-POLL])
                      r 0))))))
          (if (eq? (car result) 'start)
              (list who (aio-handle-id watcher))
              (begin (deliver (cdr result)) #f))))
      (lambda (vals) vals)
      (lambda (ss)
        (let ([node ($async-sync-slot-ref ss token #f)])
          (when node
            ($async-sync-slot-delete! ss token)
            (with-mutex (aio-handle-mutex watcher)
              (aio-queue-remove! (aio-handle-read-queue watcher) node))))
        (let ([st (aio-handle-state watcher)])
          (with-mutex (aio-state-stop-mutex st)
            (aio-state-stop-set-set! st
              (cons watcher (aio-state-stop-set st))))))))))

(define %signal-open
  (lambda (signum)
    (unless (fixnum? signum) ($oops 'signal-open "~s is not a signal" signum))
    (aio-resolve-kernel!)
    (aio-watch-open 'signal-open 'signal (format "signal ~a" signum)
      aio-signal-init signum)))

(define %signal-receive-operation
  (lambda (watcher)
    (aio-check-watcher 'signal-receive-operation watcher 'signal)
    (aio-watch-operation 'signal-receive watcher 'signal
      (lambda ()
        (aio-signal-start (aio-handle-handle watcher)
          (aio-handle-result watcher) 1)))))

(define aio-fs-event-flag-bits
  (lambda (who flags)
    (unless (and (list? flags) (for-all symbol? flags))
      ($oops who "~s is not a list of filesystem event flags" flags))
    (fold-left
      (lambda (bits flag)
        (case flag
          [(watch-entry) (fxlogior bits 1)]
          [(stat) (fxlogior bits 2)]
          [(recursive) (fxlogior bits 4)]
          [else ($oops who "~s is not a filesystem event flag" flag)]))
      0 flags)))

(define %fs-event-open
  (case-lambda
    [(path) (%fs-event-open path '())]
    [(path flags)
     (aio-check-path 'fs-event-open path)
     (aio-resolve-kernel!)
     (aio-watch-open 'fs-event-open 'fs-event path aio-fs-event-init
       (aio-fs-event-flag-bits 'fs-event-open flags))]))

(define %fs-event-receive-operation
  (lambda (watcher)
    (aio-check-watcher 'fs-event-receive-operation watcher 'fs-event)
    (aio-watch-operation 'fs-event-receive watcher 'fs-event
      (lambda ()
        (aio-fs-event-start (aio-handle-handle watcher)
          (aio-handle-path watcher) (aio-handle-result watcher))))))

(define %fs-poll-open
  (lambda (path interval)
    (aio-check-path 'fs-poll-open path)
    (unless (and (fixnum? interval) (fx> interval 0))
      ($oops 'fs-poll-open "~s is not a positive interval" interval))
    (aio-resolve-kernel!)
    (aio-watch-open 'fs-poll-open 'fs-poll path aio-fs-poll-init interval)))

(define %fs-poll-receive-operation
  (lambda (watcher)
    (aio-check-watcher 'fs-poll-receive-operation watcher 'fs-poll)
    (aio-watch-operation 'fs-poll-receive watcher 'fs-poll
      (lambda ()
        (aio-fs-poll-start (aio-handle-handle watcher)
          (aio-handle-path watcher) (aio-handle-result watcher))))))

;;; ----------------------------------------------------------------- tty

(define %tty-open
  (lambda (fd)
    (unless (and (fixnum? fd) (fx>= fd 0))
      ($oops 'tty-open "~s is not a file descriptor" fd))
    (let* ([st (aio-ensure-state! 'tty-open)]
           [id (aio-next-id st)]
           [native (aio-tty-init (aio-state-loop st) fd id)])
      (when (= native 0)
        (raise (aio-io-condition 'tty-open #f (format "fd ~a" fd)
                 'init-failed)))
      (let ([tty (make-aio-handle id native 'tty-stream st
                   (format "tty fd ~a" fd) #f (make-mutex)
                   #f #f (make-aio-queue) #f #f (make-aio-queue) #f)])
        (aio-register-handle! st tty)
        tty))))

;;; ----------------------------------------------------- system snapshots

(define aio-system-cstring
  (lambda (who proc field)
    (aio-resolve-kernel!)
    (let ([buf (make-bytevector 4096)])
      (let ([r (proc field buf 4096)])
        (if (fx< r 0)
            (raise (aio-io-condition who #f #f r))
            (bv->cstring buf))))))

(define %system-cpu-info
  (lambda ()
    (aio-resolve-kernel!)
    (let ([ctx (aio-cpu-info)])
      (when (= ctx 0)
        (raise (aio-io-condition 'system-cpu-info #f #f 'unavailable)))
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([count (aio-cpu-info-count ctx)] [buf (make-bytevector 1024)])
            (let loop ([i 0] [items '()])
              (if (fx= i count)
                  (reverse items)
                  (let ([r (aio-cpu-info-model ctx i buf 1024)])
                    (when (fx< r 0)
                      (raise (aio-io-condition 'system-cpu-info #f #f r)))
                    (loop (fx+ i 1)
                      (cons
                        (list (cons 'model (bv->cstring buf))
                              (cons 'speed (aio-cpu-info-field ctx i 0))
                              (cons 'user (aio-cpu-info-field ctx i 1))
                              (cons 'nice (aio-cpu-info-field ctx i 2))
                              (cons 'system (aio-cpu-info-field ctx i 3))
                              (cons 'idle (aio-cpu-info-field ctx i 4))
                              (cons 'irq (aio-cpu-info-field ctx i 5)))
                        items)))))))
        (lambda () (aio-cpu-info-free ctx))))))

(define %system-interface-info
  (lambda ()
    (aio-resolve-kernel!)
    (let ([ctx (aio-interface-info)])
      (when (= ctx 0)
        (raise (aio-io-condition 'system-interface-info #f #f 'unavailable)))
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([count (aio-interface-count ctx)]
                [name (make-bytevector 1024)]
                [address (make-bytevector 64)]
                [netmask (make-bytevector 64)])
            (let loop ([i 0] [items '()])
              (if (fx= i count)
                  (reverse items)
                  (let ([nr (aio-interface-name ctx i name 1024)]
                        [ar (aio-interface-address ctx i 0 address 64)]
                        [mr (aio-interface-address ctx i 1 netmask 64)]
                        [physical (make-bytevector 6)])
                    (when (or (fx< nr 0) (fx< ar 0) (fx< mr 0))
                      (raise (aio-io-condition 'system-interface-info #f #f
                               (cond [(fx< nr 0) nr] [(fx< ar 0) ar] [else mr]))))
                    (aio-interface-physical ctx i physical)
                    (loop (fx+ i 1)
                      (cons
                        (list (cons 'name (bv->cstring name))
                              (cons 'address (bv->cstring address))
                              (cons 'netmask (bv->cstring netmask))
                              (cons 'family (quotient ar 65536))
                              (cons 'internal? (not (fx= 0
                                (aio-interface-internal ctx i))))
                              (cons 'physical-address physical))
                        items)))))))
        (lambda () (aio-interface-free ctx))))))

(define %system-resource-usage
  (lambda ()
    (aio-resolve-kernel!)
    (let ([ctx (aio-rusage)])
      (when (= ctx 0)
        (raise (aio-io-condition 'system-resource-usage #f #f 'unavailable)))
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (define (field i) (aio-rusage-field ctx i))
          (list (cons 'user-time (cons (field 0) (field 1)))
                (cons 'system-time (cons (field 2) (field 3)))
                (cons 'maximum-resident-set-size (field 4))
                (cons 'minor-page-faults (field 8))
                (cons 'major-page-faults (field 9))
                (cons 'swaps (field 10))
                (cons 'input-blocks (field 11))
                (cons 'output-blocks (field 12))
                (cons 'signals (field 15))
                (cons 'voluntary-context-switches (field 16))
                (cons 'involuntary-context-switches (field 17))))
        (lambda () (aio-rusage-free ctx))))))

(define %async-loop-metrics
  (lambda ()
    (let ([st (aio-ensure-state! 'async-loop-metrics)])
      (define (field i) (aio-loop-metric (aio-state-loop st) i))
      (aio-debug-check-owner! st)
      (list (cons 'now (field 0))
            (cons 'idle-time (field 1))
            (cons 'backend-timeout (field 2))
            (cons 'backend-fd (field 3))
            (cons 'alive? (not (= (field 4) 0)))
            (cons 'loop-count (field 5))
            (cons 'events (field 6))
            (cons 'events-waiting (field 7))))))

;;; ---------------------------------------------------------------- files

;;; flag bits mirrored by aio_map_open_flags in c/asyncio.c
(define aio-open-flag-bits
  (lambda (flags)
    (fold-left
      (lambda (bits f)
        (case f
          [(read) (fxlogior bits 1)]
          [(write) (fxlogior bits 2)]
          [(create) (fxlogior bits 4)]
          [(truncate) (fxlogior bits 8)]
          [(append) (fxlogior bits 16)]
          [(exclusive) (fxlogior bits 32)]
          [else ($oops 'file-open "~s is not a file-open flag" f)]))
      0 flags)))

(define aio-serial-state
  (lambda (resource)
    (if (%async-file? resource)
        (async-file-state resource)
        (async-directory-state resource))))

(define aio-serial-mutex
  (lambda (resource)
    (if (%async-file? resource)
        (async-file-mutex resource)
        (async-directory-mutex resource))))

(define aio-serial-busy?
  (lambda (resource)
    (if (%async-file? resource)
        (async-file-busy? resource)
        (async-directory-busy? resource))))

(define aio-serial-busy?-set!
  (lambda (resource value)
    (if (%async-file? resource)
        (async-file-busy?-set! resource value)
        (async-directory-busy?-set! resource value))))

(define aio-serial-queue
  (lambda (resource)
    (if (%async-file? resource)
        (async-file-queue resource)
        (async-directory-queue resource))))

(define aio-check-serial-scope!
  (lambda (who resource)
    (if (%async-file? resource)
        (aio-check-file-scope! who resource)
        (let ([sched (current-async-scheduler)])
          (unless (and sched
                       (eq? ($async-scheduler-group-token sched)
                            ($async-scheduler-group-token
                              (aio-state-owner
                                (async-directory-state resource)))))
            ($oops who "async directory belongs to another scheduler group"))))))

(define aio-fs-request-operation
  (case-lambda
    [(who handle path start gen)
     (aio-fs-request-operation who handle path start gen
       (lambda (ss status aux) (void)) #f)]
    [(who handle path start gen canceled-gen)
     (aio-fs-request-operation who handle path start gen canceled-gen #f)]
    [(who handle path start gen canceled-gen serial-resource)
     ;; Callbacks receive ss so all mutable state belongs to this perform.
     (let ([token (list 'fs-request-operation)])
       (aio-make-operation
         (lambda (ss) #f)
         (lambda (ss deliver)
           (when serial-resource
             (aio-check-serial-scope! who serial-resource))
           (let ([st-box (box #f)] [id-box (box #f)]
                 [started? (box #f)] [entry-box (box #f)]
                 [canceled? (box #f)])
             ($async-sync-slot-set! ss token
               (vector st-box id-box started? entry-box canceled?))
             (letrec ([release!
                       (lambda ()
                         (when serial-resource
                           (let ([next
                                  (with-mutex (aio-serial-mutex serial-resource)
                                    (let ([entry
                                           (aio-queue-pop!
                                             (aio-serial-queue serial-resource))])
                                      (if (not entry)
                                          (begin
                                            (aio-serial-busy?-set! serial-resource #f)
                                            #f)
                                          (begin
                                            (set-box! (cadr entry) #t)
                                            (caddr entry)))))])
                             (when next (next)))))]
                      [finish-normal
                       (lambda (status aux)
                         (dynamic-wind
                           (lambda () (void))
                           (lambda ()
                             (if serial-resource
                                 (with-mutex (aio-serial-mutex serial-resource)
                                   (gen ss status aux))
                                 (gen ss status aux)))
                           release!))]
                      [finish-canceled
                       (lambda (status aux)
                         (dynamic-wind
                           (lambda () (void))
                           (lambda ()
                             (if serial-resource
                                 (with-mutex (aio-serial-mutex serial-resource)
                                   (canceled-gen ss status aux))
                                 (canceled-gen ss status aux)))
                           release!))]
                      [submit
                     (lambda ()
                       (let ([st (if serial-resource
                                     (aio-serial-state serial-resource)
                                     (aio-ensure-state! who))])
                       (define submit-native!
                         (lambda ()
                           (let* ([id (aio-next-id st)]
                                  [r (start ss st id)])
                             (aio-atomic-box-set-once! id-box id)
                             (if (< r 0)
                                 (cons 'error r)
                                 (begin
                                   (aio-register-request! st id
                                     (make-aio-req 'fs handle deliver r
                                       (aio-fs-finish finish-normal
                                         finish-canceled)
                                       #f))
                                   (when (or (aio-atomic-box-ref canceled?)
                                             (aio-waiter-dead? ss))
                                     (aio-cancel-request! st id))
                                   (cons 'submitted id))))))
                       (define run!
                         (lambda ()
                           (let ([result
                                  (guard (c [else (cons 'exception c)])
                                    (if serial-resource
                                        (with-mutex
                                          (aio-serial-mutex serial-resource)
                                          (if (or (aio-atomic-box-ref canceled?)
                                                  (aio-waiter-dead? ss))
                                              '(withdrawn)
                                              (submit-native!)))
                                        (if (or (aio-atomic-box-ref canceled?)
                                                (aio-waiter-dead? ss))
                                            '(withdrawn)
                                            (submit-native!))))])
                             (case (car result)
                               [(submitted) (void)]
                               [(withdrawn) (release!)]
                               [(error)
                                (release!)
                                (deliver
                                  (cons 'raise
                                    (aio-io-condition who handle path
                                      (cdr result))))]
                               [else
                                (release!)
                                (deliver (cons 'raise (cdr result)))]))))
                         (aio-atomic-box-set-once! st-box st)
                         (if (aio-run-on-owner! st run!)
                             (list 'fs who)
                             (begin
                               (release!)
                               (deliver
                                 (cons 'raise
                                   (aio-io-condition who handle path 'closed)))
                               #f))))])
             (if serial-resource
                 (let ([entry (list ss started? submit)] [start-now? #f])
                   (set-box! entry-box entry)
                   (with-mutex (aio-serial-mutex serial-resource)
                     (if (aio-serial-busy? serial-resource)
                         (set-box! entry-box
                           (aio-queue-push!
                             (aio-serial-queue serial-resource) entry))
                         (begin
                           (aio-serial-busy?-set! serial-resource #t)
                           (set-box! started? #t)
                           (set! start-now? #t))))
                   (when start-now? (submit))
                   (list 'file who))
                 (submit)))))
         (lambda (vals) vals)
         (lambda (ss)
           (let ([attempt ($async-sync-slot-ref ss token #f)])
             (when attempt
               (let ([st-box (vector-ref attempt 0)]
                     [id-box (vector-ref attempt 1)]
                     [started? (vector-ref attempt 2)]
                     [entry-box (vector-ref attempt 3)]
                     [nack-started? (not serial-resource)])
                 (aio-atomic-box-flag! (vector-ref attempt 4))
                 (when serial-resource
                   (with-mutex (aio-serial-mutex serial-resource)
                     (if (unbox started?)
                         (set! nack-started? #t)
                         (let ([node (unbox entry-box)])
                           (when node
                             (aio-queue-remove!
                               (aio-serial-queue serial-resource) node))))))
                 (when nack-started?
                   (let ([st (aio-atomic-box-ref st-box)]
                         [id (aio-atomic-box-ref id-box)])
                     (when (and st id) (aio-cancel-request! st id))))))))))]))

(define %file-open-operation
  (case-lambda
    [(path flags) (%file-open-operation path flags #o666)]
    [(path flags mode)
     (unless (string? path)
       ($oops 'file-open-operation "~s is not a string" path))
     (unless (and (list? flags) (for-all symbol? flags))
       ($oops 'file-open-operation "~s is not a list of flag symbols" flags))
     (unless (and (fixnum? mode) (fx>= mode 0) (fx<= mode #o7777))
       ($oops 'file-open-operation "~s is not a valid file mode" mode))
     (let ([bits (aio-open-flag-bits flags)]
           [token (list 'file-open-operation)])
       (aio-fs-request-operation 'open #f path
         (lambda (ss st id)
           ($async-sync-slot-set! ss token st)
           (aio-fs-open (aio-state-loop st) path bits mode id))
         (lambda (ss status aux)
           (if (>= status 0)
               (let ([st ($async-sync-slot-ref ss token #f)])
                 (let ([f
                        (make-async-file% status path st #f
                          (if (fxlogtest bits 16) -1 0) ; append: track end lazily
                          #f (make-mutex) #f (make-aio-queue))])
                   (cons 'values (list (aio-register-file! st f)))))
               (cons 'raise (aio-io-condition 'open #f path status))))
         (lambda (ss status aux)
           (when (>= status 0)
             (aio-fs-close-now status)))))]))

(define aio-check-file/raw
  (lambda (who f)
    (when (async-file-closed? f)
      (raise (aio-io-condition who f (async-file-path f) 'closed)))))

(define aio-check-file
  (lambda (who f)
    (unless (%async-file? f)
      ($oops who "~s is not an async file" f))
    (with-mutex (async-file-mutex f)
      (aio-check-file/raw who f))))

(define aio-check-file-unowned
  (lambda (who f)
    (unless (%async-file? f)
      ($oops who "~s is not an async file" f))
    (with-mutex (async-file-mutex f)
      (aio-check-file/raw who f)
      (when (async-file-port-owned? f)
        ($oops who "async file ownership has been transferred to a port")))))

(define aio-check-file-unowned/raw
  (lambda (who f)
    (aio-check-file/raw who f)
    (when (async-file-port-owned? f)
      ($oops who "async file ownership has been transferred to a port"))))

(define aio-check-file-scope!
  (lambda (who f)
    (let ([sched (current-async-scheduler)])
      (unless (and sched
                   (eq? ($async-scheduler-group-token sched)
                        ($async-scheduler-group-token
                          (aio-state-owner (async-file-state f)))))
        ($oops who "async file belongs to another scheduler group")))))

(define aio-claim-file-for-port!
  (lambda (who f)
    (unless (%async-file? f)
      ($oops who "~s is not an async file" f))
    (aio-check-file-scope! who f)
    (with-mutex (async-file-mutex f)
      (aio-check-file/raw who f)
      (when (async-file-port-owned? f)
        ($oops who "async file ownership has already been transferred to a port"))
      (async-file-port-owned?-set! f #t))))

(define aio-file-offset!
  (lambda (f n)
    ;; an offset of -1 means the append/current position
    (let ([off (async-file-offset f)])
      (when (and (>= n 0) (>= off 0))
        (async-file-offset-set! f (+ off n)))
      off)))

(define %file-read-operation
  (lambda (f len . allow-owned-option)
    (aio-check-file 'file-read-operation f)
    (unless (and (fixnum? len) (fx> len 0) (fx<= len (expt 2 30)))
      ($oops 'file-read-operation "~s is not a positive fixnum length" len))
    (let ([allow-owned? (and (pair? allow-owned-option)
                             (car allow-owned-option))])
      (aio-fs-request-operation 'read f (async-file-path f)
      (lambda (ss st id)
        (if allow-owned?
            (aio-check-file/raw 'file-read-operation f)
            (aio-check-file-unowned/raw 'file-read-operation f))
        (aio-fs-read (aio-state-loop st) (async-file-fd f) len
          (aio-file-offset! f 0) id))
      (lambda (ss status aux)
        (cond
          [(fx> status 0)
           (aio-file-offset! f status)
           (let ([bv (make-bytevector status)])
             (aio-read-copy (aio-fs-data aux) bv status)
             (cons 'values (list bv)))]
          [(fx= status 0) (cons 'values (list #!eof))]
          [else
           (cons 'raise
             (aio-io-condition 'read f (async-file-path f) status))]))
      (lambda (ss status aux)
        (when (fx>= status 0) (aio-file-offset! f status)))
      f))))

(define %file-write-operation
  (lambda (f bv . allow-owned-option)
    (aio-check-file 'file-write-operation f)
    (unless (bytevector? bv)
      ($oops 'file-write-operation "~s is not a bytevector" bv))
    (let ([allow-owned? (and (pair? allow-owned-option)
                             (car allow-owned-option))])
      (aio-fs-request-operation 'write f (async-file-path f)
      (lambda (ss st id)
        (if allow-owned?
            (aio-check-file/raw 'file-write-operation f)
            (aio-check-file-unowned/raw 'file-write-operation f))
        (aio-fs-write (aio-state-loop st) (async-file-fd f) bv
          (bytevector-length bv) (aio-file-offset! f 0) id))
      (lambda (ss status aux)
        (if (>= status 0)
            (begin
              (aio-file-offset! f status)
              (cons 'values (list status)))
            (cons 'raise
              (aio-io-condition 'write f (async-file-path f) status))))
      (lambda (ss status aux)
        (when (fx>= status 0) (aio-file-offset! f status)))
      f))))

(define %file-close-operation
  (lambda (f)
    (aio-fs-request-operation 'close f (async-file-path f)
      (lambda (ss st id)
        (aio-fs-close-fd (aio-state-loop st) (async-file-fd f) id))
      (lambda (ss status aux)
        (if (fx= status 0)
            (begin
              (async-file-closed?-set! f #t)
              (aio-unregister-file! f)
              (cons 'values '()))
            (cons 'raise
              (aio-io-condition 'close f (async-file-path f) status))))
      (lambda (ss status aux)
        (when (fx= status 0)
          (async-file-closed?-set! f #t)
          (aio-unregister-file! f)))
      f)))

(define aio-stat-alist
  (lambda (aux)
    (define (field i) (aio-fs-stat-field aux i))
    (list
      (cons 'dev (field 0))
      (cons 'mode (field 1))
      (cons 'nlink (field 2))
      (cons 'uid (field 3))
      (cons 'gid (field 4))
      (cons 'rdev (field 5))
      (cons 'ino (field 6))
      (cons 'size (field 7))
      (cons 'blksize (field 8))
      (cons 'blocks (field 9))
      (cons 'atime (cons (field 12) (field 13)))
      (cons 'mtime (cons (field 14) (field 15)))
      (cons 'ctime (cons (field 16) (field 17))))))

;;; ------------------------------------------------------- port adapters

(define aio-port-read-procedure
  (lambda (read-chunk)
    (let ([pending #f] [pending-start 0])
      (lambda (target start count)
        (if (fx= count 0)
            0
            (let loop ()
              (if pending
                  (let* ([available
                          (fx- (bytevector-length pending) pending-start)]
                         [n (fxmin available count)])
                    (bytevector-copy! pending pending-start target start n)
                    (if (fx= n available)
                        (begin
                          (set! pending #f)
                          (set! pending-start 0))
                        (set! pending-start (fx+ pending-start n)))
                    n)
                  (let ([chunk (read-chunk count)])
                    (cond
                      [(eof-object? chunk) 0]
                      [(fx= (bytevector-length chunk) 0) (loop)]
                      [else
                       (set! pending chunk)
                       (set! pending-start 0)
                       (loop)])))))))))

(define aio-port-write-procedure
  (lambda (write-chunk)
    (lambda (source start count)
      (if (fx= count 0)
          0
          (let* ([chunk
                  (if (and (fx= start 0)
                           (fx= count (bytevector-length source)))
                      source
                      (let ([copy (make-bytevector count)])
                        (bytevector-copy! source start copy 0 count)
                        copy))]
                 [n (write-chunk chunk)])
            (when (fx= n 0)
              ($oops 'async-handle-port "write made no progress"))
            n)))))

(define aio-stream-port-read-procedure
  (lambda (s)
    (aio-port-read-procedure
      (lambda (count)
        (perform-operation (%stream-read-operation s #t))))))

(define aio-stream-port-write-procedure
  (lambda (s)
    (aio-port-write-procedure
      (lambda (bv)
        (let ([n (bytevector-length bv)])
          (perform-operation (%stream-write-operation s bv #t))
          n)))))

(define aio-file-port-read-procedure
  (lambda (f)
    (aio-port-read-procedure
      (lambda (count)
        (perform-operation (%file-read-operation f count #t))))))

(define aio-file-port-write-procedure
  (lambda (f)
    (aio-port-write-procedure
      (lambda (bv)
        (perform-operation (%file-write-operation f bv #t))))))

(define aio-file-port-position-procedures
  (lambda (f)
    (if (with-mutex (async-file-mutex f)
          (fx>= (async-file-offset f) 0))
        (values
          (lambda ()
            (with-mutex (async-file-mutex f)
              (aio-check-file/raw 'port-position f)
              (async-file-offset f)))
          (lambda (position)
            (with-mutex (async-file-mutex f)
              (aio-check-file/raw 'set-port-position! f)
              (async-file-offset-set! f position))))
        (values #f #f))))

(define aio-close-file-from-port
  (lambda (f)
    (aio-check-file 'close-port f)
    (perform-operation (%file-close-operation f))))

(define aio-file-port-id
  (lambda (f)
    (format "async file ~a" (async-file-path f))))

(define aio-stream-port-id
  (lambda (s)
    (let ([path (aio-handle-path s)])
      (if path
          (format "async stream ~a" path)
          "async stream"))))

(define %file-stat-operation
  (lambda (target)
    (cond
      [(string? target)
       (aio-fs-request-operation 'stat #f target
         (lambda (ss st id) (aio-fs-stat (aio-state-loop st) target id))
         (lambda (ss status aux)
           (if (fx= status 0)
               (cons 'values (list (aio-stat-alist aux)))
               (cons 'raise (aio-io-condition 'stat #f target status)))))]
      [(%async-file? target)
       (aio-check-file 'file-stat-operation target)
       (aio-fs-request-operation 'stat target (async-file-path target)
         (lambda (ss st id)
           (aio-check-file-scope! 'file-stat-operation target)
           (aio-check-file-unowned/raw 'file-stat-operation target)
           (aio-fs-fstat (aio-state-loop st) (async-file-fd target) id))
         (lambda (ss status aux)
           (if (fx= status 0)
               (cons 'values (list (aio-stat-alist aux)))
               (cons 'raise
                 (aio-io-condition 'stat target (async-file-path target) status))))
         (lambda (ss status aux) (void))
         target)]
      [else
       ($oops 'file-stat-operation "~s is not a path or async file" target)])))

(define aio-check-path
  (lambda (who path)
    (unless (string? path) ($oops who "~s is not a string" path))))

(define aio-check-mode
  (lambda (who mode)
    (unless (and (fixnum? mode) (fx>= mode 0) (fx<= mode #o7777))
      ($oops who "~s is not a valid file mode" mode))))

(define aio-fs-void-operation
  (lambda (who handle path start . maybe-file)
    (aio-fs-request-operation who handle path start
      (lambda (ss status aux)
        (if (fx>= status 0)
            (cons 'values '())
            (cons 'raise (aio-io-condition who handle path status))))
      (lambda (ss status aux) (void))
      (and (pair? maybe-file) (car maybe-file)))))

(define aio-fs-result-cstring
  (lambda (aux length copy who path)
    (let ([n (length aux)])
      (if (fx< n 0)
          (raise (aio-io-condition who #f path n))
          (let ([bv (make-bytevector (fx+ n 1))])
            (let ([r (copy aux bv (fx+ n 1))])
              (if (fx< r 0)
                  (raise (aio-io-condition who #f path r))
                  (bv->cstring bv))))))))

(define %file-delete-operation
  (lambda (path)
    (aio-check-path 'file-delete-operation path)
    (aio-fs-void-operation 'delete #f path
      (lambda (ss st id) (aio-fs-unlink (aio-state-loop st) path id)))))

(define %file-rename-operation
  (lambda (old new)
    (aio-check-path 'file-rename-operation old)
    (aio-check-path 'file-rename-operation new)
    (aio-fs-void-operation 'rename #f old
      (lambda (ss st id)
        (aio-fs-rename (aio-state-loop st) old new id)))))

(define %directory-create-operation
  (case-lambda
    [(path) (%directory-create-operation path #o755)]
    [(path mode)
     (aio-check-path 'directory-create-operation path)
     (aio-check-mode 'directory-create-operation mode)
     (aio-fs-void-operation 'mkdir #f path
       (lambda (ss st id)
         (aio-fs-mkdir (aio-state-loop st) path mode id)))]))

(define %directory-delete-operation
  (lambda (path)
    (aio-check-path 'directory-delete-operation path)
    (aio-fs-void-operation 'rmdir #f path
      (lambda (ss st id) (aio-fs-rmdir (aio-state-loop st) path id)))))

(define aio-copy-flag-bits
  (lambda (who flags)
    (unless (and (list? flags) (for-all symbol? flags))
      ($oops who "~s is not a list of copy flags" flags))
    (fold-left
      (lambda (bits flag)
        (case flag
          [(exclusive) (fxlogior bits 1)]
          [(clone) (fxlogior bits 2)]
          [(clone-force) (fxlogior bits 4)]
          [else ($oops who "~s is not a copy flag" flag)]))
      0 flags)))

(define %file-copy-operation
  (case-lambda
    [(from to) (%file-copy-operation from to '())]
    [(from to flags)
     (aio-check-path 'file-copy-operation from)
     (aio-check-path 'file-copy-operation to)
     (let ([bits (aio-copy-flag-bits 'file-copy-operation flags)])
       (aio-fs-void-operation 'copy #f from
         (lambda (ss st id)
           (aio-fs-copyfile (aio-state-loop st) from to bits id))))]))

(define %temporary-directory-create-operation
  (lambda (pattern)
    (aio-check-path 'temporary-directory-create-operation pattern)
    (aio-fs-request-operation 'mkdtemp #f pattern
      (lambda (ss st id) (aio-fs-mkdtemp (aio-state-loop st) pattern id))
      (lambda (ss status aux)
        (if (fx= status 0)
            (cons 'values
              (list (aio-fs-result-cstring aux aio-fs-result-path-length
                      aio-fs-result-path-copy 'mkdtemp pattern)))
            (cons 'raise (aio-io-condition 'mkdtemp #f pattern status)))))))

(define %temporary-file-open-operation
  (lambda (pattern)
    (aio-check-path 'temporary-file-open-operation pattern)
    (let ([token (list 'temporary-file-open-operation)])
      (aio-fs-request-operation 'mkstemp #f pattern
        (lambda (ss st id)
          ($async-sync-slot-set! ss token st)
          (aio-fs-mkstemp (aio-state-loop st) pattern id))
        (lambda (ss status aux)
          (if (fx>= status 0)
              (let* ([st ($async-sync-slot-ref ss token #f)]
                     [path (aio-fs-result-cstring aux
                             aio-fs-result-path-length aio-fs-result-path-copy
                             'mkstemp pattern)]
                     [f (make-async-file% status path st #f 0 #f
                          (make-mutex) #f (make-aio-queue))])
                (cons 'values (list (aio-register-file! st f) path)))
              (cons 'raise (aio-io-condition 'mkstemp #f pattern status))))
        (lambda (ss status aux)
          (when (fx>= status 0) (aio-fs-close-now status)))))))

(define aio-dirent-type
  (lambda (n)
    (case n
      [(1) 'unknown]
      [(2) 'file]
      [(3) 'directory]
      [(4) 'link]
      [(5) 'fifo]
      [(6) 'socket]
      [(7) 'character-device]
      [(8) 'block-device]
      [else 'unknown])))

(define %directory-scan-operation
  (lambda (path)
    (aio-check-path 'directory-scan-operation path)
    (aio-fs-request-operation 'scandir #f path
      (lambda (ss st id) (aio-fs-scandir (aio-state-loop st) path id))
      (lambda (ss status aux)
        (if (fx>= status 0)
            (let ([buf (make-bytevector 4097)])
              (let loop ([entries '()])
                (let ([type (aio-fs-scandir-next aux buf 4097)])
                  (cond
                    [(fx= type 0) (cons 'values (list (reverse entries)))]
                    [(fx< type 0)
                     (cons 'raise (aio-io-condition 'scandir #f path type))]
                    [else
                     (loop (cons (cons (bv->cstring buf)
                                      (aio-dirent-type type))
                                  entries))]))))
            (cons 'raise (aio-io-condition 'scandir #f path status)))))))

(define aio-check-directory/raw
  (lambda (who directory)
    (when (async-directory-closed? directory)
      (raise
        (aio-io-condition who directory (async-directory-path directory)
          'closed)))))

(define aio-check-directory
  (lambda (who directory)
    (unless (%async-directory? directory)
      ($oops who "~s is not an async directory" directory))
    (with-mutex (async-directory-mutex directory)
      (aio-check-directory/raw who directory))))

(define %directory-open-operation
  (lambda (path)
    (aio-check-path 'directory-open-operation path)
    (let ([token (list 'directory-open-operation)])
      (aio-fs-request-operation 'opendir #f path
        (lambda (ss st id)
          ($async-sync-slot-set! ss token st)
          (aio-fs-opendir (aio-state-loop st) path id))
        (lambda (ss status aux)
          (if (fx>= status 0)
              (let* ([st ($async-sync-slot-ref ss token #f)]
                     [directory
                      (make-async-directory% (aio-fs-result-ptr aux) path st
                        #f (make-mutex) #f (make-aio-queue))])
                (cons 'values
                  (list (aio-register-directory! st directory))))
              (cons 'raise (aio-io-condition 'opendir #f path status))))
        (lambda (ss status aux)
          (when (fx>= status 0)
            (aio-fs-closedir-now (aio-fs-result-ptr aux))))))))

(define %directory-read-operation
  (case-lambda
    [(directory) (%directory-read-operation directory 64)]
    [(directory count)
     (aio-check-directory 'directory-read-operation directory)
     (unless (and (fixnum? count) (fx> count 0))
       ($oops 'directory-read-operation
         "~s is not a positive entry count" count))
     (aio-fs-request-operation 'readdir directory
       (async-directory-path directory)
       (lambda (ss st id)
         (aio-check-directory/raw 'directory-read-operation directory)
         (aio-fs-readdir (aio-state-loop st)
           (async-directory-pointer directory) count id))
       (lambda (ss status aux)
         (if (fx>= status 0)
             (let ([buf (make-bytevector 4097)])
               (let loop ([i 0] [entries '()])
                 (if (fx= i status)
                     (cons 'values (list (reverse entries)))
                     (let ([type (aio-fs-readdir-entry aux i buf 4097)])
                       (if (fx< type 0)
                           (cons 'raise
                             (aio-io-condition 'readdir directory
                               (async-directory-path directory) type))
                           (loop (fx+ i 1)
                             (cons (cons (bv->cstring buf)
                                         (aio-dirent-type type))
                                   entries)))))))
             (cons 'raise
               (aio-io-condition 'readdir directory
                 (async-directory-path directory) status))))
       (lambda (ss status aux) (void))
       directory)]))

(define aio-directory-close-complete!
  (lambda (directory status)
    (when (fx>= status 0)
      (async-directory-closed?-set! directory #t)
      (aio-unregister-directory! directory))))

(define %directory-close-operation
  (lambda (directory)
    (aio-check-directory 'directory-close-operation directory)
    (aio-fs-request-operation 'closedir directory
      (async-directory-path directory)
      (lambda (ss st id)
        (aio-check-directory/raw 'directory-close-operation directory)
        (aio-fs-closedir (aio-state-loop st)
          (async-directory-pointer directory) id))
      (lambda (ss status aux)
        (aio-directory-close-complete! directory status)
        (if (fx>= status 0)
            (cons 'values (list (void)))
            (cons 'raise
              (aio-io-condition 'closedir directory
                (async-directory-path directory) status))))
      (lambda (ss status aux)
        (aio-directory-close-complete! directory status))
      directory)))

(define %file-sync-operation
  (lambda (f data-only?)
    (aio-check-file-unowned (if data-only? 'file-data-sync-operation
                                'file-sync-operation) f)
    (let ([who (if data-only? 'fdatasync 'fsync)])
      (aio-fs-void-operation who f (async-file-path f)
        (lambda (ss st id)
          ((if data-only? aio-fs-fdatasync aio-fs-fsync)
           (aio-state-loop st) (async-file-fd f) id))
        f))))

(define %file-truncate-operation
  (lambda (f length)
    (aio-check-file-unowned 'file-truncate-operation f)
    (unless (and (integer? length) (exact? length) (>= length 0))
      ($oops 'file-truncate-operation "~s is not an exact nonnegative length"
        length))
    (aio-fs-void-operation 'truncate f (async-file-path f)
      (lambda (ss st id)
        (aio-fs-ftruncate (aio-state-loop st) (async-file-fd f) length id))
      f)))

(define %file-send-operation
  (lambda (output input offset length)
    (aio-check-file-unowned 'file-send-operation output)
    (aio-check-file-unowned 'file-send-operation input)
    (when (eq? output input)
      ($oops 'file-send-operation "input and output files are the same"))
    (unless (and (integer? offset) (exact? offset) (>= offset 0))
      ($oops 'file-send-operation "~s is not a nonnegative offset" offset))
    (unless (and (integer? length) (exact? length) (>= length 0))
      ($oops 'file-send-operation "~s is not a nonnegative length" length))
    (aio-fs-request-operation 'sendfile output (async-file-path input)
      (lambda (ss st id)
        (aio-check-file-scope! 'file-send-operation input)
        (with-mutex (async-file-mutex input)
          (aio-check-file-unowned/raw 'file-send-operation input)
          (aio-fs-sendfile (aio-state-loop st)
            (async-file-fd output) (async-file-fd input) offset length id)))
      (lambda (ss status aux)
        (if (fx>= status 0)
            (begin
              (aio-file-offset! output status)
              (cons 'values (list status)))
            (cons 'raise
              (aio-io-condition 'sendfile output (async-file-path input)
                status))))
      (lambda (ss status aux) (void))
      output)))

(define aio-access-mode-bits
  (lambda (who modes)
    (unless (and (list? modes) (for-all symbol? modes))
      ($oops who "~s is not a list of access modes" modes))
    (fold-left
      (lambda (bits mode)
        (case mode
          [(exists) bits]
          [(read) (fxlogior bits 1)]
          [(write) (fxlogior bits 2)]
          [(execute) (fxlogior bits 4)]
          [else ($oops who "~s is not an access mode" mode)]))
      0 modes)))

(define %file-access-operation
  (lambda (path modes)
    (aio-check-path 'file-access-operation path)
    (let ([bits (aio-access-mode-bits 'file-access-operation modes)])
      (aio-fs-request-operation 'access #f path
        (lambda (ss st id)
          (aio-fs-access (aio-state-loop st) path bits id))
        (lambda (ss status aux) (cons 'values (list (fx= status 0))))))))

(define %file-mode-set-operation
  (lambda (target mode)
    (aio-check-mode 'file-mode-set-operation mode)
    (cond
      [(string? target)
       (aio-fs-void-operation 'chmod #f target
         (lambda (ss st id)
           (aio-fs-chmod (aio-state-loop st) target mode id)))]
      [(%async-file? target)
       (aio-check-file-unowned 'file-mode-set-operation target)
       (aio-fs-void-operation 'fchmod target (async-file-path target)
         (lambda (ss st id)
           (aio-fs-fchmod (aio-state-loop st) (async-file-fd target) mode id))
         target)]
      [else ($oops 'file-mode-set-operation
              "~s is not a path or async file" target)])))

(define aio-check-time
  (lambda (who value)
    (unless (real? value) ($oops who "~s is not a real timestamp" value))
    (if (inexact? value) value (exact->inexact value))))

(define %file-times-set-operation
  (case-lambda
    [(target atime mtime) (%file-times-set-operation target atime mtime #t)]
    [(target atime mtime follow?)
     (let ([a (aio-check-time 'file-times-set-operation atime)]
           [m (aio-check-time 'file-times-set-operation mtime)])
       (cond
         [(string? target)
          (aio-fs-void-operation (if follow? 'utime 'lutime) #f target
            (lambda (ss st id)
              (aio-fs-utime (aio-state-loop st) target a m
                (if follow? 1 0) id)))]
         [(%async-file? target)
          (aio-check-file-unowned 'file-times-set-operation target)
          (aio-fs-void-operation 'futime target (async-file-path target)
            (lambda (ss st id)
              (aio-fs-futime (aio-state-loop st) (async-file-fd target)
                a m id))
            target)]
         [else ($oops 'file-times-set-operation
                 "~s is not a path or async file" target)]))]))

(define %file-lstat-operation
  (lambda (path)
    (aio-check-path 'file-lstat-operation path)
    (aio-fs-request-operation 'lstat #f path
      (lambda (ss st id) (aio-fs-lstat (aio-state-loop st) path id))
      (lambda (ss status aux)
        (if (fx= status 0)
            (cons 'values (list (aio-stat-alist aux)))
            (cons 'raise (aio-io-condition 'lstat #f path status)))))))

(define %file-link-operation
  (lambda (from to symbolic? flags)
    (aio-check-path 'file-link-operation from)
    (aio-check-path 'file-link-operation to)
    (let ([bits
           (fold-left
             (lambda (bits flag)
               (case flag
                 [(directory) (fxlogior bits 1)]
                 [(junction) (fxlogior bits 2)]
                 [else ($oops 'file-link-operation
                         "~s is not a symbolic-link flag" flag)]))
             0 flags)])
      (aio-fs-void-operation (if symbolic? 'symlink 'link) #f from
        (lambda (ss st id)
          (aio-fs-link (aio-state-loop st) from to
            (if symbolic? 1 0) bits id))))))

(define %file-read-link-operation
  (lambda (path realpath?)
    (aio-check-path 'file-read-link-operation path)
    (let ([who (if realpath? 'realpath 'readlink)])
      (aio-fs-request-operation who #f path
        (lambda (ss st id)
          (aio-fs-readlink (aio-state-loop st) path
            (if realpath? 1 0) id))
        (lambda (ss status aux)
          (if (fx= status 0)
              (cons 'values
                (list (aio-fs-result-cstring aux aio-fs-result-string-length
                        aio-fs-result-string-copy who path)))
              (cons 'raise (aio-io-condition who #f path status))))))))

(define %file-owner-set-operation
  (case-lambda
    [(target uid gid) (%file-owner-set-operation target uid gid #t)]
    [(target uid gid follow?)
     (unless (and (integer? uid) (exact? uid) (>= uid 0))
       ($oops 'file-owner-set-operation "~s is not a uid" uid))
     (unless (and (integer? gid) (exact? gid) (>= gid 0))
       ($oops 'file-owner-set-operation "~s is not a gid" gid))
     (cond
       [(string? target)
        (aio-fs-void-operation (if follow? 'chown 'lchown) #f target
          (lambda (ss st id)
            (aio-fs-chown (aio-state-loop st) target -1 uid gid
              (if follow? 0 2) id)))]
       [(%async-file? target)
        (aio-check-file-unowned 'file-owner-set-operation target)
        (aio-fs-void-operation 'fchown target (async-file-path target)
          (lambda (ss st id)
            (aio-fs-chown (aio-state-loop st) "" (async-file-fd target)
              uid gid 1 id))
          target)]
       [else ($oops 'file-owner-set-operation
               "~s is not a path or async file" target)])]))

(define aio-statfs-alist
  (lambda (aux)
    (define (field i) (aio-fs-statfs-field aux i))
    (list (cons 'type (field 0))
          (cons 'block-size (field 1))
          (cons 'blocks (field 2))
          (cons 'blocks-free (field 3))
          (cons 'blocks-available (field 4))
          (cons 'files (field 5))
          (cons 'files-free (field 6))
          (cons 'fragment-size (field 7)))))

(define %file-system-stat-operation
  (lambda (path)
    (aio-check-path 'file-system-stat-operation path)
    (aio-fs-request-operation 'statfs #f path
      (lambda (ss st id) (aio-fs-statfs (aio-state-loop st) path id))
      (lambda (ss status aux)
        (if (fx= status 0)
            (cons 'values (list (aio-statfs-alist aux)))
            (cons 'raise (aio-io-condition 'statfs #f path status)))))))

;;; ------------------------------------------------------- public exports

(set! make-async-io-condition
  (lambda (operation handle path code)
    (make-async-io-condition% operation handle path code)))

(set! async-io-condition? %async-io-condition?)
(set! async-io-condition-operation %async-io-condition-operation)
(set! async-io-condition-handle %async-io-condition-handle)
(set! async-io-condition-path %async-io-condition-path)
(set! async-io-condition-code %async-io-condition-code)

(set-who! async-io-error-name
  (lambda (code)
    (unless (fixnum? code) ($oops who "~s is not a fixnum error code" code))
    (aio-resolve-kernel!)
    (let ([buf (make-bytevector 64)])
      (aio-err-name-into code buf 64)
      (bv->cstring buf))))

(set-who! async-io-error-message
  (lambda (code)
    (unless (fixnum? code) ($oops who "~s is not a fixnum error code" code))
    (aio-resolve-kernel!)
    (let ([buf (make-bytevector 256)])
      (aio-strerror-into code buf 256)
      (bv->cstring buf))))

(set! tcp-listener?
  (lambda (x)
    (and (aio-handle? x)
         (or (eq? (aio-handle-kind x) 'tcp-listener)
             (eq? (aio-handle-kind x) 'pipe-listener)))))
(set-who! tcp-listener-close
  (lambda (listener)
    (unless (tcp-listener? listener)
      ($oops who "~s is not a listener" listener))
    (aio-close-owned-handle who listener)))
(set-who! tcp-accept
  (lambda (listener)
    (perform-operation (%tcp-accept-operation listener))))
(set-who! tcp-connect
  (lambda (host port)
    (perform-operation (%tcp-connect-operation host port))))
(set-who! pipe-connect
  (lambda (path)
    (perform-operation (%pipe-connect-operation path))))

(set! async-stream?
  (lambda (x)
    (and (aio-handle? x)
         (or (eq? (aio-handle-kind x) 'tcp-stream)
             (eq? (aio-handle-kind x) 'pipe-stream)
             (eq? (aio-handle-kind x) 'tty-stream)))))
(set! tcp-stream?
  (lambda (x)
    (and (aio-handle? x) (eq? (aio-handle-kind x) 'tcp-stream))))
(set! pipe-stream?
  (lambda (x)
    (and (aio-handle? x) (eq? (aio-handle-kind x) 'pipe-stream))))
(set! tty-stream?
  (lambda (x)
    (and (aio-handle? x) (eq? (aio-handle-kind x) 'tty-stream))))
(set-who! stream-close
  (lambda (s)
    (aio-check-stream-unowned who s)
    (aio-close-owned-handle who s)))
(set! stream-closed?
  (lambda (s)
    (and (aio-handle? s)
         (with-mutex (aio-handle-mutex s)
           (or (aio-handle-closing? s) (aio-handle-closed? s))))))
(set-who! stream-read
  (lambda (s)
    (aio-check-stream-unowned who s)
    (perform-operation (%stream-read-operation s))))
(set-who! stream-write
  (lambda (s bv)
    (aio-check-stream-unowned who s)
    (perform-operation (%stream-write-operation s bv))
    (void)))
(set-who! stream-shutdown
  (lambda (s)
    (aio-check-stream-unowned who s)
    (perform-operation (stream-shutdown-operation s))
    (void)))
(set-who! async-stream->binary-input-port
  (lambda (s)
    (aio-claim-stream-for-port! who s)
    (make-custom-binary-input-port (aio-stream-port-id s)
      (aio-stream-port-read-procedure s)
      #f #f
      (lambda () (aio-close-owned-handle who s)))))
(set-who! async-stream->binary-output-port
  (lambda (s)
    (aio-claim-stream-for-port! who s)
    (make-custom-binary-output-port (aio-stream-port-id s)
      (aio-stream-port-write-procedure s)
      #f #f
      (lambda () (aio-close-owned-handle who s)))))
(set-who! async-stream->binary-input/output-port
  (lambda (s)
    (aio-claim-stream-for-port! who s)
    (let ([p
           (make-custom-binary-input/output-port (aio-stream-port-id s)
             (aio-stream-port-read-procedure s)
             (aio-stream-port-write-procedure s)
             #f #f
             (lambda () (aio-close-owned-handle who s)))])
      ;; A stream cannot seek backward over input prefetched by a duplex port.
      ($reset-port-flags! p (constant port-flag-block-buffered))
      p)))

(set! dns-lookup
  (case-lambda
    [(node) (perform-operation (%dns-lookup-operation node))]
    [(node service) (perform-operation (%dns-lookup-operation node service))]))
(set! dns-reverse-operation %dns-reverse-operation)
(set! dns-reverse
  (case-lambda
    [(host port) (perform-operation (%dns-reverse-operation host port))]
    [(host port flags)
     (perform-operation (%dns-reverse-operation host port flags))]))
(set! random-bytevector-operation %random-bytevector-operation)
(set-who! random-bytevector
  (lambda (length)
    (perform-operation (%random-bytevector-operation length))))
(set! fd-poll-open %fd-poll-open)
(set! fd-poll-handle?
  (lambda (x) (and (aio-handle? x) (eq? (aio-handle-kind x) 'poll))))
(set-who! fd-poll-close
  (lambda (poll)
    (unless (fd-poll-handle? poll)
      ($oops who "~s is not an fd poll handle" poll))
    (aio-close-owned-handle who poll)))
(set! fd-poll-operation %fd-poll-operation)
(set-who! fd-poll
  (lambda (poll events)
    (perform-operation (%fd-poll-operation poll events))))
(set! process-spawn %process-spawn)
(set! async-process?
  (lambda (x) (and (aio-handle? x) (eq? (aio-handle-kind x) 'process))))
(set-who! process-id
  (lambda (process)
    (unless (async-process? process)
      ($oops who "~s is not an async process" process))
    (aio-process-pid (aio-handle-handle process))))
(set! process-wait-operation %process-wait-operation)
(set-who! process-wait
  (lambda (process)
    (perform-operation (%process-wait-operation process))))
(set-who! process-kill
  (lambda (process signal)
    (unless (async-process? process)
      ($oops who "~s is not an async process" process))
    (unless (fixnum? signal) ($oops who "~s is not a signal number" signal))
    (aio-check-handle-scope! who process)
    (let ([r (aio-process-kill (aio-handle-handle process) signal)])
      (when (fx< r 0)
        (raise (aio-io-condition 'process-kill process
                 (aio-handle-path process) r))))
    (void)))
(set-who! process-id-kill
  (lambda (pid signal)
    (unless (and (integer? pid) (exact? pid))
      ($oops who "~s is not a process id" pid))
    (unless (fixnum? signal) ($oops who "~s is not a signal number" signal))
    (let ([r (aio-kill pid signal)])
      (when (fx< r 0)
        (raise (aio-io-condition 'process-kill #f #f r))))
    (void)))
(set-who! process-close
  (lambda (process)
    (unless (async-process? process)
      ($oops who "~s is not an async process" process))
    (aio-close-owned-handle who process)))
(set! signal-open %signal-open)
(set! signal-watcher?
  (lambda (x) (and (aio-handle? x) (eq? (aio-handle-kind x) 'signal))))
(set! signal-receive-operation %signal-receive-operation)
(set-who! signal-receive
  (lambda (watcher)
    (perform-operation (%signal-receive-operation watcher))))
(set-who! signal-close
  (lambda (watcher)
    (aio-check-watcher who watcher 'signal)
    (aio-close-owned-handle who watcher)))
(set! fs-event-open %fs-event-open)
(set! fs-event-watcher?
  (lambda (x) (and (aio-handle? x) (eq? (aio-handle-kind x) 'fs-event))))
(set! fs-event-receive-operation %fs-event-receive-operation)
(set-who! fs-event-receive
  (lambda (watcher)
    (perform-operation (%fs-event-receive-operation watcher))))
(set-who! fs-event-close
  (lambda (watcher)
    (aio-check-watcher who watcher 'fs-event)
    (aio-close-owned-handle who watcher)))
(set! fs-poll-open %fs-poll-open)
(set! fs-poll-watcher?
  (lambda (x) (and (aio-handle? x) (eq? (aio-handle-kind x) 'fs-poll))))
(set! fs-poll-receive-operation %fs-poll-receive-operation)
(set-who! fs-poll-receive
  (lambda (watcher)
    (perform-operation (%fs-poll-receive-operation watcher))))
(set-who! fs-poll-close
  (lambda (watcher)
    (aio-check-watcher who watcher 'fs-poll)
    (aio-close-owned-handle who watcher)))
(set! tty-open %tty-open)
(set-who! tty-mode-set!
  (lambda (tty mode)
    (unless (tty-stream? tty) ($oops who "~s is not a TTY stream" tty))
    (let ([value (case mode [(normal) 0] [(raw) 1] [(io) 2]
                   [else ($oops who "~s is not a TTY mode" mode)])])
      (aio-check-handle-scope! who tty)
      (let ([r (aio-tty-set-mode (aio-handle-handle tty) value)])
        (when (fx< r 0)
          (raise (aio-io-condition who tty (aio-handle-path tty) r))))
      (void))))
(set-who! tty-window-size
  (lambda (tty)
    (unless (tty-stream? tty) ($oops who "~s is not a TTY stream" tty))
    (aio-check-handle-scope! who tty)
    (let ([width (aio-tty-winsize (aio-handle-handle tty) 0)]
          [height (aio-tty-winsize (aio-handle-handle tty) 1)])
      (when (fx< width 0)
        (raise (aio-io-condition who tty (aio-handle-path tty) width)))
      (when (fx< height 0)
        (raise (aio-io-condition who tty (aio-handle-path tty) height)))
      (values width height))))
(set-who! tty-virtual-terminal-state
  (lambda ()
    (aio-resolve-kernel!)
    (let ([r (aio-tty-get-vterm-state)])
      (if (fx< r 0)
          (raise (aio-io-condition who #f #f r))
          (if (fx= r 0) 'unsupported 'supported)))))
(set-who! tty-virtual-terminal-state-set!
  (lambda (state)
    (unless (memq state '(supported unsupported))
      ($oops who "~s is not a virtual terminal state" state))
    (aio-resolve-kernel!)
    (aio-tty-set-vterm-state (if (eq? state 'supported) 1 0))
    (void)))
(set-who! tty-reset-mode!
  (lambda ()
    (aio-resolve-kernel!)
    (aio-tty-reset-mode)
    (void)))
(set-who! system-high-resolution-time
  (lambda () (aio-resolve-kernel!) (aio-system-u64 0)))
(set-who! system-memory-info
  (lambda ()
    (aio-resolve-kernel!)
    (list (cons 'total (aio-system-u64 1))
          (cons 'free (aio-system-u64 2))
          (cons 'constrained (aio-system-u64 3))
          (cons 'available (aio-system-u64 4))
          (cons 'resident-set (aio-system-u64 5)))))
(set-who! system-uptime
  (lambda () (aio-resolve-kernel!) (aio-system-double 0)))
(set-who! system-load-average
  (lambda ()
    (aio-resolve-kernel!)
    (list (aio-system-double 1) (aio-system-double 2)
          (aio-system-double 3))))
(set-who! system-process-info
  (lambda ()
    (aio-resolve-kernel!)
    (list (cons 'pid (aio-system-u64 6))
          (cons 'parent-pid (aio-system-u64 7))
          (cons 'available-parallelism (aio-system-u64 8)))))
(set-who! system-path-info
  (lambda ()
    (list (cons 'executable (aio-system-cstring who aio-system-string 0))
          (cons 'current-directory
            (aio-system-cstring who aio-system-string 1))
          (cons 'home-directory
            (aio-system-cstring who aio-system-string 2))
          (cons 'temporary-directory
            (aio-system-cstring who aio-system-string 3))
          (cons 'hostname (aio-system-cstring who aio-system-string 4)))))
(set-who! system-uname
  (lambda ()
    (list (cons 'system (aio-system-cstring who aio-uname-string 0))
          (cons 'release (aio-system-cstring who aio-uname-string 1))
          (cons 'version (aio-system-cstring who aio-uname-string 2))
          (cons 'machine (aio-system-cstring who aio-uname-string 3)))))
(set-who! system-cpu-info (lambda () (%system-cpu-info)))
(set-who! system-interface-info (lambda () (%system-interface-info)))
(set-who! system-resource-usage (lambda () (%system-resource-usage)))
(set-who! async-loop-metrics (lambda () (%async-loop-metrics)))

(set! udp-open %udp-open)
(set! udp-socket?
  (lambda (x) (and (aio-handle? x) (eq? (aio-handle-kind x) 'udp))))
(set-who! udp-close
  (lambda (socket)
    (aio-check-udp who socket)
    (aio-close-owned-handle who socket)))
(set! udp-send-operation %udp-send-operation)
(set! udp-send
  (case-lambda
    [(socket bv) (perform-operation (%udp-send-operation socket bv))]
    [(socket bv host port)
     (perform-operation (%udp-send-operation socket bv host port))]))
(set! udp-receive-operation %udp-receive-operation)
(set-who! udp-receive
  (lambda (socket) (perform-operation (%udp-receive-operation socket))))
(set! udp-bind!
  (case-lambda
    [(socket host port) (udp-bind! socket host port '())]
    [(socket host port flags)
     (aio-check-host-port 'udp-bind! host port)
     (aio-udp-control 'udp-bind socket
       (lambda ()
         (aio-udp-bind (aio-handle-handle socket) host port
           (aio-udp-bind-flag-bits 'udp-bind! flags))))
     (void)]))
(set-who! udp-connect!
  (lambda (socket host port)
    (aio-check-host-port who host port)
    (aio-udp-control 'udp-connect socket
      (lambda ()
        (aio-udp-connect (aio-handle-handle socket) host port)))
    (void)))
(set-who! udp-disconnect!
  (lambda (socket)
    (aio-udp-control 'udp-disconnect socket
      (lambda () (aio-udp-connect (aio-handle-handle socket) "" 0)))
    (void)))
(set-who! udp-local-address
  (lambda (socket) (apply values (aio-udp-address-list who socket #f))))
(set-who! udp-peer-address
  (lambda (socket) (apply values (aio-udp-address-list who socket #t))))
(set-who! udp-membership-set!
  (lambda (socket multicast interface source action)
    (unless (string? multicast) ($oops who "~s is not a string" multicast))
    (unless (string? interface) ($oops who "~s is not a string" interface))
    (unless (string? source) ($oops who "~s is not a string" source))
    (unless (memq action '(join leave))
      ($oops who "~s is not a membership action" action))
    (aio-udp-control who socket
      (lambda ()
        (aio-udp-set-membership (aio-handle-handle socket)
          multicast interface source (if (eq? action 'join) 1 0))))
    (void)))
(set-who! udp-multicast-interface-set!
  (lambda (socket interface)
    (unless (string? interface) ($oops who "~s is not a string" interface))
    (aio-udp-control who socket
      (lambda ()
        (aio-udp-set-multicast-interface (aio-handle-handle socket)
          interface)))
    (void)))
(set-who! udp-option-set!
  (lambda (socket option value)
    (let ([index
           (case option
             [(multicast-loop) 0]
             [(multicast-ttl) 1]
             [(broadcast) 3]
             [(ttl) 4]
             [else ($oops who "~s is not a UDP option" option)])])
      (unless (and (integer? value) (exact? value))
        ($oops who "~s is not an exact integer" value))
      (aio-udp-control who socket
        (lambda ()
          (aio-udp-set-option (aio-handle-handle socket) index value)))
      (void))))

(set! async-file? %async-file?)
(set! file-open
  (case-lambda
    [(path flags) (perform-operation (%file-open-operation path flags))]
    [(path flags mode) (perform-operation (%file-open-operation path flags mode))]))
(set-who! file-read
  (lambda (f len)
    (aio-check-file-unowned who f)
    (perform-operation (%file-read-operation f len))))
(set-who! file-write
  (lambda (f bv)
    (aio-check-file-unowned who f)
    (perform-operation (%file-write-operation f bv))))
(set-who! file-close
  (lambda (f)
    (aio-check-file-unowned who f)
    (perform-operation (%file-close-operation f))
    (void)))
(set-who! async-file->binary-input-port
  (lambda (f)
    (aio-claim-file-for-port! who f)
    (let-values ([(get-position set-position!)
                  (aio-file-port-position-procedures f)])
      (make-custom-binary-input-port (aio-file-port-id f)
        (aio-file-port-read-procedure f)
        get-position set-position!
        (lambda () (aio-close-file-from-port f))))))
(set-who! async-file->binary-output-port
  (lambda (f)
    (aio-claim-file-for-port! who f)
    (let-values ([(get-position set-position!)
                  (aio-file-port-position-procedures f)])
      (make-custom-binary-output-port (aio-file-port-id f)
        (aio-file-port-write-procedure f)
        get-position set-position!
        (lambda () (aio-close-file-from-port f))))))
(set-who! async-file->binary-input/output-port
  (lambda (f)
    (aio-claim-file-for-port! who f)
    (let-values ([(get-position set-position!)
                  (aio-file-port-position-procedures f)])
      (make-custom-binary-input/output-port (aio-file-port-id f)
        (aio-file-port-read-procedure f)
        (aio-file-port-write-procedure f)
        get-position set-position!
        (lambda () (aio-close-file-from-port f))))))
(set-who! file-stat
  (lambda (target)
    (when (%async-file? target)
      (aio-check-file-unowned who target))
    (perform-operation (%file-stat-operation target))))
(set-who! file-delete
  (lambda (path)
    (perform-operation (%file-delete-operation path))
    (void)))
(set-who! file-rename
  (lambda (old new)
    (perform-operation (%file-rename-operation old new))
    (void)))
(set-who! directory-create
  (case-lambda
    [(path) (directory-create path #o755)]
    [(path mode)
     (perform-operation (%directory-create-operation path mode))
     (void)]))
(set-who! directory-delete
  (lambda (path)
    (perform-operation (%directory-delete-operation path))
    (void)))
(set! file-copy
  (case-lambda
    [(from to) (perform-operation (%file-copy-operation from to))]
    [(from to flags)
     (perform-operation (%file-copy-operation from to flags))]
    ))
(set-who! temporary-directory-create
  (lambda (pattern)
    (perform-operation (%temporary-directory-create-operation pattern))))
(set-who! temporary-file-open
  (lambda (pattern)
    (perform-operation (%temporary-file-open-operation pattern))))
(set-who! directory-scan
  (lambda (path) (perform-operation (%directory-scan-operation path))))
(set! async-directory? %async-directory?)
(set-who! directory-open
  (lambda (path) (perform-operation (%directory-open-operation path))))
(set! directory-read
  (case-lambda
    [(directory) (perform-operation (%directory-read-operation directory))]
    [(directory count)
     (perform-operation (%directory-read-operation directory count))]))
(set-who! directory-close
  (lambda (directory)
    (perform-operation (%directory-close-operation directory))
    (void)))
(set-who! file-sync
  (lambda (f)
    (perform-operation (%file-sync-operation f #f))
    (void)))
(set-who! file-data-sync
  (lambda (f)
    (perform-operation (%file-sync-operation f #t))
    (void)))
(set-who! file-truncate
  (lambda (f length)
    (perform-operation (%file-truncate-operation f length))
    (void)))
(set! file-send-operation %file-send-operation)
(set-who! file-send
  (lambda (output input offset length)
    (perform-operation (%file-send-operation output input offset length))))
(set-who! file-access?
  (lambda (path modes)
    (perform-operation (%file-access-operation path modes))))
(set-who! file-mode-set!
  (lambda (target mode)
    (perform-operation (%file-mode-set-operation target mode))
    (void)))
(set! file-times-set!
  (case-lambda
    [(target atime mtime)
     (perform-operation (%file-times-set-operation target atime mtime))
     (void)]
    [(target atime mtime follow?)
     (perform-operation
       (%file-times-set-operation target atime mtime follow?))
     (void)]))
(set-who! file-lstat
  (lambda (path) (perform-operation (%file-lstat-operation path))))
(set-who! file-link
  (lambda (from to)
    (perform-operation (%file-link-operation from to #f '()))
    (void)))
(set! file-symbolic-link
  (case-lambda
    [(from to) (file-symbolic-link from to '())]
    [(from to flags)
     (perform-operation (%file-link-operation from to #t flags))
     (void)]))
(set-who! file-read-link
  (lambda (path)
    (perform-operation (%file-read-link-operation path #f))))
(set-who! file-real-path
  (lambda (path)
    (perform-operation (%file-read-link-operation path #t))))
(set! file-owner-set!
  (case-lambda
    [(target uid gid)
     (perform-operation (%file-owner-set-operation target uid gid))
     (void)]
    [(target uid gid follow?)
     (perform-operation (%file-owner-set-operation target uid gid follow?))
     (void)]))
(set-who! file-system-stat
  (lambda (path)
    (perform-operation (%file-system-stat-operation path))))

(set! tcp-listen %tcp-listen)
(set! tcp-accept-operation %tcp-accept-operation)
(set! tcp-connect-operation %tcp-connect-operation)
(set! pipe-listen %pipe-listen)
(set! pipe-connect-operation %pipe-connect-operation)
(set-who! stream-read-operation
  (lambda (s)
    (aio-check-stream-unowned who s)
    (%stream-read-operation s)))
(set-who! stream-write-operation
  (lambda (s bv)
    (aio-check-stream-unowned who s)
    (%stream-write-operation s bv)))
(set! dns-lookup-operation %dns-lookup-operation)
(set! file-open-operation %file-open-operation)
(set! file-close-operation %file-close-operation)
(set-who! file-read-operation
  (lambda (f len)
    (aio-check-file-unowned who f)
    (%file-read-operation f len)))
(set-who! file-write-operation
  (lambda (f bv)
    (aio-check-file-unowned who f)
    (%file-write-operation f bv)))
(set-who! file-stat-operation
  (lambda (target)
    (when (%async-file? target)
      (aio-check-file-unowned who target))
    (%file-stat-operation target)))
(set! file-delete-operation %file-delete-operation)
(set! file-rename-operation %file-rename-operation)
(set! directory-create-operation %directory-create-operation)
(set! directory-delete-operation %directory-delete-operation)
(set! file-copy-operation %file-copy-operation)
(set! temporary-directory-create-operation
  %temporary-directory-create-operation)
(set! temporary-file-open-operation %temporary-file-open-operation)
(set! directory-scan-operation %directory-scan-operation)
(set! directory-open-operation %directory-open-operation)
(set! directory-read-operation %directory-read-operation)
(set! directory-close-operation %directory-close-operation)
(set-who! file-sync-operation
  (lambda (f) (%file-sync-operation f #f)))
(set-who! file-data-sync-operation
  (lambda (f) (%file-sync-operation f #t)))
(set! file-truncate-operation %file-truncate-operation)
(set! file-access-operation %file-access-operation)
(set! file-mode-set-operation %file-mode-set-operation)
(set! file-times-set-operation %file-times-set-operation)
(set! file-lstat-operation %file-lstat-operation)
(set-who! file-link-operation
  (lambda (from to) (%file-link-operation from to #f '())))
(set! file-symbolic-link-operation
  (case-lambda
    [(from to) (%file-link-operation from to #t '())]
    [(from to flags) (%file-link-operation from to #t flags)]))
(set-who! file-read-link-operation
  (lambda (path) (%file-read-link-operation path #f)))
(set-who! file-real-path-operation
  (lambda (path) (%file-read-link-operation path #t)))
(set! file-owner-set-operation %file-owner-set-operation)
(set! file-system-stat-operation %file-system-stat-operation)

;;; the scheduler calls this when run-async exits
(set! $async-io-shutdown aio-io-shutdown)

(record-writer (type-descriptor aio-handle)
  (lambda (r p wr)
    (let ([closed?
           (with-mutex (aio-handle-mutex r)
             (aio-handle-closed? r))])
      (fprintf p "#<async-~a ~a~a>"
        (aio-handle-kind r)
        (or (aio-handle-path r) (aio-handle-id r))
        (if closed? " closed" "")))))

(record-writer (type-descriptor async-file)
  (lambda (r p wr)
    (let ([closed?
           (with-mutex (async-file-mutex r)
             (async-file-closed? r))])
      (fprintf p "#<async-file ~a~a>"
        (async-file-path r)
        (if closed? " closed" "")))))
)
