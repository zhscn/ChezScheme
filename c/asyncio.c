/* asyncio.c - libuv shim for Chez Scheme async I/O.

   This shim owns the concrete layouts of uv_loop_t, uv_handle_t, and
   uv_req_t.  Scheme code refers to native objects through opaque pointers
   and stable integer identifiers registered in a Scheme-side registry.

   All user-visible events are appended to a loop-local completion queue.
   Scheme drains that queue after uv_run returns, so libuv worker threads and
   callbacks never enter Scheme. */

#if !defined(_WIN32) && !defined(_POSIX_C_SOURCE)
# define _POSIX_C_SOURCE 200112L
#endif

#include <uv.h>
#include "scheme.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stddef.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#if !defined(_WIN32)
# include <unistd.h>
#else
# include <io.h>
#endif

/* event kinds delivered to the notify trampoline */
#define AIO_EV_ACCEPT    1  /* listener became readable: id = listener id */
#define AIO_EV_READ      2  /* stream read: status nread (>0), UV_EOF, or error */
#define AIO_EV_WRITE     3  /* status 0 or error */
#define AIO_EV_CONNECT   4  /* outbound connect: status 0 or error */
#define AIO_EV_SHUTDOWN  5
#define AIO_EV_FS        6  /* status = uv_fs_get_result */
#define AIO_EV_DNS       7  /* status 0 or error; aux = request context */
#define AIO_EV_CLOSE     8  /* handle close completed: id = handle id */
#define AIO_EV_UDP_RECV  9
#define AIO_EV_UDP_SEND 10
#define AIO_EV_NAMEINFO 11
#define AIO_EV_RANDOM   12
#define AIO_EV_POLL     13
#define AIO_EV_PROCESS  14
#define AIO_EV_SIGNAL   15
#define AIO_EV_FS_EVENT 16
#define AIO_EV_FS_POLL  17

typedef struct aio_loop aio_loop_t;

typedef struct aio_completion {
  struct aio_completion *next;
  int64_t id;
  int64_t kind;
  int64_t status;
  void *aux;
} aio_completion_t;

typedef struct aio_read_buffer {
  struct aio_read_buffer *next;
  aio_loop_t *owner;
  char data[65536];
} aio_read_buffer_t;

struct aio_loop {
  uv_loop_t *loop;
  uv_async_t *wakeup;    /* lets foreign threads interrupt a blocking uv_run */
  uv_timer_t *bridge;    /* fires at the nearest Scheme-side timer deadline */
  aio_read_buffer_t *read_buffers;
  unsigned int read_buffer_count;
  aio_completion_t *completion_head;
  aio_completion_t *completion_tail;
};

/* per-request context for operations that carry C-owned data */
typedef struct {
  uv_fs_t fs;            /* also used as generic storage for fs requests */
  char *buf;             /* read destination or write copy, freed via aio_fs_buf_free */
  uv_dirent_t dirent;    /* current scandir entry */
  uv_dirent_t *dirents;  /* readdir batch, owned by the request */
  size_t dirent_count;
  uv_dir_t *dir;         /* directory whose temporary batch fields we set */
  int temp_fd1;
  int temp_fd2;
} aio_fs_ctx_t;

typedef struct {
  uv_getaddrinfo_t gai;
  char *node;
  char *service;
} aio_dns_ctx_t;

typedef struct {
  uv_write_t wreq;
  char *buf;
} aio_write_ctx_t;

typedef struct {
  uv_connect_t creq;
} aio_connect_ctx_t;

typedef struct {
  uv_shutdown_t sreq;
} aio_shutdown_ctx_t;

typedef struct {
  uv_connect_t creq;
} aio_pconnect_ctx_t;

typedef struct {
  char *buf;
  struct sockaddr_storage peer;
} aio_udp_recv_ctx_t;

typedef struct {
  uv_udp_send_t req;
  char *buf;
} aio_udp_send_ctx_t;

typedef struct {
  uv_getnameinfo_t req;
  char *host;
  char *service;
} aio_nameinfo_ctx_t;

typedef struct {
  uv_random_t req;
  char *buf;
  size_t len;
} aio_random_ctx_t;

typedef struct {
  int64_t term_signal;
} aio_process_exit_t;

typedef struct {
  char *filename;
  int events;
} aio_fs_event_result_t;

typedef struct {
  uv_stat_t previous;
  uv_stat_t current;
} aio_fs_poll_result_t;

static aio_loop_t *aio_loop_of(uv_loop_t *loop) {
  return (aio_loop_t *)uv_loop_get_data(loop);
}

static char *aio_read_buffer_take(aio_loop_t *al) {
  aio_read_buffer_t *buffer = al->read_buffers;
  if (buffer) {
    al->read_buffers = buffer->next;
    al->read_buffer_count--;
  } else {
    buffer = (aio_read_buffer_t *)malloc(sizeof(*buffer));
    if (!buffer) return NULL;
    buffer->owner = al;
  }
  buffer->next = NULL;
  return buffer->data;
}

static void aio_read_buffer_return(char *data) {
  aio_read_buffer_t *buffer;
  aio_loop_t *al;
  if (!data) return;
  buffer = (aio_read_buffer_t *)(data - offsetof(aio_read_buffer_t, data));
  al = buffer->owner;
  if (al->read_buffer_count < 32) {
    buffer->next = al->read_buffers;
    al->read_buffers = buffer;
    al->read_buffer_count++;
  } else {
    free(buffer);
  }
}

static void aio_report(uv_loop_t *loop, int64_t id, int64_t kind,
                       int64_t status, void *aux) {
  aio_loop_t *al = aio_loop_of(loop);
  aio_completion_t *completion =
    (aio_completion_t *)malloc(sizeof(aio_completion_t));
  if (!completion) {
    fputs("async I/O completion queue allocation failed\n", stderr);
    abort();
  }
  completion->next = NULL;
  completion->id = id;
  completion->kind = kind;
  completion->status = status;
  completion->aux = aux;
  if (al->completion_tail)
    al->completion_tail->next = completion;
  else
    al->completion_head = completion;
  al->completion_tail = completion;
}

static char *aio_strdup(const char *s) {
  size_t n = strlen(s) + 1;
  char *copy = (char *)malloc(n);
  if (copy) memcpy(copy, s, n);
  return copy;
}

/* ------------------------------------------------------------- loop ----- */

void *aio_loop_open(void) {
  aio_loop_t *al = (aio_loop_t *)malloc(sizeof(aio_loop_t));
  if (!al) return NULL;
  al->loop = (uv_loop_t *)malloc(sizeof(uv_loop_t));
  if (!al->loop || uv_loop_init(al->loop) != 0) {
    free(al->loop);
    free(al);
    return NULL;
  }
  al->wakeup = NULL;
  al->bridge = NULL;
  al->read_buffers = NULL;
  al->read_buffer_count = 0;
  al->completion_head = NULL;
  al->completion_tail = NULL;
  uv_loop_set_data(al->loop, al);
  (void)uv_loop_configure(al->loop, UV_METRICS_IDLE_TIME);
  return al;
}

/* Writes id, kind, status, and aux as four native int64 values. */
int aio_completion_pop(void *al_, unsigned char *out) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_completion_t *completion = al->completion_head;
  int64_t values[4];
  if (!completion) return 0;
  al->completion_head = completion->next;
  if (!al->completion_head) al->completion_tail = NULL;
  values[0] = completion->id;
  values[1] = completion->kind;
  values[2] = completion->status;
  values[3] = (int64_t)(intptr_t)completion->aux;
  memcpy(out, values, sizeof(values));
  free(completion);
  return 1;
}

/* mode: 0 = poll without waiting, 1 = wait for one event */
int aio_loop_run(void *al_, int mode) {
  aio_loop_t *al = (aio_loop_t *)al_;
  return uv_run(al->loop, mode ? UV_RUN_ONCE : UV_RUN_NOWAIT);
}

int aio_loop_alive(void *al_) {
  aio_loop_t *al = (aio_loop_t *)al_;
  return uv_loop_alive(al->loop);
}

/* closes the loop after all handles are closed; frees the wrapper */
int aio_loop_destroy(void *al_) {
  aio_loop_t *al = (aio_loop_t *)al_;
  int r;
  if (al->completion_head) return UV_EBUSY;
  r = uv_loop_close(al->loop);
  if (r == 0) {
    while (al->read_buffers) {
      aio_read_buffer_t *buffer = al->read_buffers;
      al->read_buffers = buffer->next;
      free(buffer);
    }
    free(al->loop);
    free(al);
  }
  return r;
}

static void aio_noop_async_cb(uv_async_t *h) { (void)h; }
static void aio_noop_timer_cb(uv_timer_t *h) { (void)h; }

/* create the cross-thread wakeup handle; returns NULL on failure */
void *aio_wakeup_init(void *al_) {
  aio_loop_t *al = (aio_loop_t *)al_;
  uv_async_t *h = (uv_async_t *)malloc(sizeof(uv_async_t));
  if (!h) return NULL;
  if (uv_async_init(al->loop, h, aio_noop_async_cb) != 0) {
    free(h);
    return NULL;
  }
  al->wakeup = h;
  return h;
}

int aio_wakeup_send(void *al_) {
  aio_loop_t *al = (aio_loop_t *)al_;
  if (!al->wakeup) return UV_EINVAL;
  return uv_async_send(al->wakeup);
}

/* bridge timer: wakes a blocking uv_run when a Scheme timer comes due */
void *aio_bridge_init(void *al_) {
  aio_loop_t *al = (aio_loop_t *)al_;
  uv_timer_t *h = (uv_timer_t *)malloc(sizeof(uv_timer_t));
  if (!h) return NULL;
  if (uv_timer_init(al->loop, h) != 0) {
    free(h);
    return NULL;
  }
  al->bridge = h;
  return h;
}

int aio_bridge_start(void *al_, int64_t timeout_ms) {
  aio_loop_t *al = (aio_loop_t *)al_;
  if (!al->bridge) return UV_EINVAL;
  return uv_timer_start(al->bridge, aio_noop_timer_cb,
                        timeout_ms < 0 ? 0 : (uint64_t)timeout_ms, 0);
}

int aio_bridge_stop(void *al_) {
  aio_loop_t *al = (aio_loop_t *)al_;
  if (!al->bridge) return UV_EINVAL;
  return uv_timer_stop(al->bridge);
}

/* ---------------------------------------------------------- handles ----- */

/* generic close: the close callback reports AIO_EV_CLOSE and frees the
   handle storage */
static void aio_handle_close_cb(uv_handle_t *h) {
  uv_loop_t *loop = h->loop;
  int64_t id = (int64_t)(intptr_t)uv_handle_get_data(h);
  aio_report(loop, id, AIO_EV_CLOSE, 0, NULL);
  free(h);
}

void aio_handle_close(void *h) {
  if (h && !uv_is_closing((uv_handle_t *)h))
    uv_close((uv_handle_t *)h, aio_handle_close_cb);
}

int aio_handle_is_closing(void *h) {
  return uv_is_closing((uv_handle_t *)h);
}

/* ------------------------------------------------------------- tcp ------ */

/* returns a new tcp handle registered under id, or NULL */
void *aio_tcp_init(void *al_, int64_t id) {
  aio_loop_t *al = (aio_loop_t *)al_;
  uv_tcp_t *h = (uv_tcp_t *)malloc(sizeof(uv_tcp_t));
  if (!h) return NULL;
  if (uv_tcp_init(al->loop, h) != 0) {
    free(h);
    return NULL;
  }
  uv_handle_set_data((uv_handle_t *)h, (void *)(intptr_t)id);
  return h;
}

static int aio_parse_addr(const char *host, int64_t port,
                          struct sockaddr_storage *addr) {
  int r = uv_ip4_addr(host, (int)port, (struct sockaddr_in *)addr);
  if (r == 0) return 0;
  return uv_ip6_addr(host, (int)port, (struct sockaddr_in6 *)addr);
}

int aio_tcp_bind(void *h, const char *host, int64_t port) {
  struct sockaddr_storage addr;
  int r = aio_parse_addr(host, port, &addr);
  if (r != 0) return r;
  return uv_tcp_bind((uv_tcp_t *)h, (const struct sockaddr *)&addr, 0);
}

static void aio_connection_cb(uv_stream_t *server, int status) {
  int64_t id = (int64_t)(intptr_t)uv_handle_get_data((uv_handle_t *)server);
  aio_report(server->loop, id, AIO_EV_ACCEPT, status, NULL);
}

int aio_listen_start(void *h, int64_t backlog) {
  return uv_listen((uv_stream_t *)h, (int)backlog, aio_connection_cb);
}

int aio_accept(void *server, void *client) {
  return uv_accept((uv_stream_t *)server, (uv_stream_t *)client);
}

static void aio_connect_cb(uv_connect_t *req, int status) {
  aio_connect_ctx_t *ctx = (aio_connect_ctx_t *)req;
  int64_t id = (int64_t)(intptr_t)uv_req_get_data((uv_req_t *)req);
  aio_report(req->handle->loop, id, AIO_EV_CONNECT, status, NULL);
  free(ctx);
}

int aio_tcp_connect(void *h, const char *host, int64_t port, int64_t reqid) {
  struct sockaddr_storage addr;
  aio_connect_ctx_t *ctx;
  int r = aio_parse_addr(host, port, &addr);
  if (r != 0) return r;
  ctx = (aio_connect_ctx_t *)malloc(sizeof(aio_connect_ctx_t));
  if (!ctx) return UV_ENOMEM;
  uv_req_set_data((uv_req_t *)&ctx->creq, (void *)(intptr_t)reqid);
  r = uv_tcp_connect(&ctx->creq, (uv_tcp_t *)h,
                     (const struct sockaddr *)&addr, aio_connect_cb);
  if (r != 0) free(ctx);
  return r;
}

/* -------------------------------------------------- local-domain pipes -- */

void *aio_pipe_init(void *al_, int64_t id) {
  aio_loop_t *al = (aio_loop_t *)al_;
  uv_pipe_t *h = (uv_pipe_t *)malloc(sizeof(uv_pipe_t));
  if (!h) return NULL;
  if (uv_pipe_init(al->loop, h, 0) != 0) {
    free(h);
    return NULL;
  }
  uv_handle_set_data((uv_handle_t *)h, (void *)(intptr_t)id);
  return h;
}

int aio_pipe_bind(void *h, const char *path) {
  return uv_pipe_bind((uv_pipe_t *)h, path);
}

static void aio_pconnect_cb(uv_connect_t *req, int status) {
  aio_pconnect_ctx_t *ctx = (aio_pconnect_ctx_t *)req;
  int64_t id = (int64_t)(intptr_t)uv_req_get_data((uv_req_t *)req);
  aio_report(req->handle->loop, id, AIO_EV_CONNECT, status, NULL);
  free(ctx);
}

int aio_pipe_connect(void *h, const char *path, int64_t reqid) {
  aio_pconnect_ctx_t *ctx =
      (aio_pconnect_ctx_t *)malloc(sizeof(aio_pconnect_ctx_t));
  if (!ctx) return UV_ENOMEM;
  uv_req_set_data((uv_req_t *)&ctx->creq, (void *)(intptr_t)reqid);
  uv_pipe_connect(&ctx->creq, (uv_pipe_t *)h, path, aio_pconnect_cb);
  return 0;
}

/* ---------------------------------------------------------- streams ----- */

/* one-shot reads: each read request arms the stream; the first data,
   EOF, or error disarms it and completes the request */
static void aio_read_alloc_cb(uv_handle_t *h, size_t suggested,
                              uv_buf_t *buf) {
  (void)suggested;
  buf->base = aio_read_buffer_take(aio_loop_of(h->loop));
  buf->len = buf->base ? 65536 : 0;
}

static void aio_read_cb(uv_stream_t *stream, ssize_t nread,
                        const uv_buf_t *buf) {
  int64_t id = (int64_t)(intptr_t)uv_handle_get_data((uv_handle_t *)stream);
  if (nread == 0) {
    aio_read_buffer_return(buf->base);
    return;
  }
  uv_read_stop(stream);
  if (nread > 0) {
    aio_report(stream->loop, id, AIO_EV_READ, nread, buf->base);
  } else {
    aio_read_buffer_return(buf->base);
    aio_report(stream->loop, id, AIO_EV_READ, nread, NULL);
  }
}

/* copy n bytes from a completed read buffer; caller then frees it */
void aio_read_copy(void *buf, void *dest, int64_t n) {
  memcpy(dest, buf, (size_t)n);
}

void aio_free(void *p) { aio_read_buffer_return((char *)p); }

int aio_read_start(void *h) {
  return uv_read_start((uv_stream_t *)h, aio_read_alloc_cb, aio_read_cb);
}

int aio_read_stop(void *h) {
  return uv_read_stop((uv_stream_t *)h);
}

static void aio_write_cb(uv_write_t *req, int status) {
  aio_write_ctx_t *ctx = (aio_write_ctx_t *)req;
  int64_t id = (int64_t)(intptr_t)uv_req_get_data((uv_req_t *)req);
  aio_report(req->handle->loop, id, AIO_EV_WRITE, status, NULL);
  free(ctx->buf);
  free(ctx);
}

/* copies len bytes from src so the Scheme side need not pin its buffer */
static int chez_aio_write(void *h, const void *src, int64_t len, int64_t reqid) {
  aio_write_ctx_t *ctx;
  uv_buf_t buf;
  int r;
  if (len < 0 || (uint64_t)len > UINT_MAX) return UV_EINVAL;
  ctx = (aio_write_ctx_t *)malloc(sizeof(aio_write_ctx_t));
  if (!ctx) return UV_ENOMEM;
  ctx->buf = (char *)malloc((size_t)len ? (size_t)len : 1);
  if (!ctx->buf) {
    free(ctx);
    return UV_ENOMEM;
  }
  memcpy(ctx->buf, src, (size_t)len);
  uv_req_set_data((uv_req_t *)&ctx->wreq, (void *)(intptr_t)reqid);
  buf = uv_buf_init(ctx->buf, (unsigned int)len);
  r = uv_write(&ctx->wreq, (uv_stream_t *)h, &buf, 1, aio_write_cb);
  if (r != 0) {
    free(ctx->buf);
    free(ctx);
  }
  return r;
}

static void aio_shutdown_cb(uv_shutdown_t *req, int status) {
  aio_shutdown_ctx_t *ctx = (aio_shutdown_ctx_t *)req;
  int64_t id = (int64_t)(intptr_t)uv_req_get_data((uv_req_t *)req);
  aio_report(req->handle->loop, id, AIO_EV_SHUTDOWN, status, NULL);
  free(ctx);
}

int aio_shutdown(void *h, int64_t reqid) {
  aio_shutdown_ctx_t *ctx =
      (aio_shutdown_ctx_t *)malloc(sizeof(aio_shutdown_ctx_t));
  if (!ctx) return UV_ENOMEM;
  uv_req_set_data((uv_req_t *)&ctx->sreq, (void *)(intptr_t)reqid);
  int r = uv_shutdown(&ctx->sreq, (uv_stream_t *)h, aio_shutdown_cb);
  if (r != 0) free(ctx);
  return r;
}

/* ------------------------------------------------------------- udp ------ */

void *aio_udp_init(void *al_, int64_t id) {
  aio_loop_t *al = (aio_loop_t *)al_;
  uv_udp_t *h = (uv_udp_t *)malloc(sizeof(uv_udp_t));
  if (!h) return NULL;
  if (uv_udp_init(al->loop, h) != 0) {
    free(h);
    return NULL;
  }
  uv_handle_set_data((uv_handle_t *)h, (void *)(intptr_t)id);
  return h;
}

int aio_udp_bind(void *h, const char *host, int64_t port, int flags) {
  struct sockaddr_storage addr;
  unsigned int uv_flags = 0;
  int r = aio_parse_addr(host, port, &addr);
  if (r != 0) return r;
  if (flags & 1) uv_flags |= UV_UDP_IPV6ONLY;
  if (flags & 2) uv_flags |= UV_UDP_REUSEADDR;
  return uv_udp_bind((uv_udp_t *)h, (const struct sockaddr *)&addr, uv_flags);
}

int aio_udp_connect(void *h, const char *host, int64_t port) {
  struct sockaddr_storage addr;
  int r;
  if (!host || !*host) return uv_udp_connect((uv_udp_t *)h, NULL);
  r = aio_parse_addr(host, port, &addr);
  if (r != 0) return r;
  return uv_udp_connect((uv_udp_t *)h, (const struct sockaddr *)&addr);
}

static void aio_udp_alloc_cb(uv_handle_t *h, size_t suggested, uv_buf_t *buf) {
  (void)suggested;
  buf->base = aio_read_buffer_take(aio_loop_of(h->loop));
  buf->len = buf->base ? 65536 : 0;
}

static void aio_udp_recv_cb(uv_udp_t *h, ssize_t nread, const uv_buf_t *buf,
                            const struct sockaddr *addr, unsigned flags) {
  int64_t id = (int64_t)(intptr_t)uv_handle_get_data((uv_handle_t *)h);
  aio_udp_recv_ctx_t *ctx;
  (void)flags;
  if (nread == 0 && addr == NULL) {
    aio_read_buffer_return(buf->base);
    return;
  }
  uv_udp_recv_stop(h);
  if (nread < 0) {
    aio_read_buffer_return(buf->base);
    aio_report(h->loop, id, AIO_EV_UDP_RECV, nread, NULL);
    return;
  }
  ctx = (aio_udp_recv_ctx_t *)malloc(sizeof(*ctx));
  if (!ctx) {
    aio_read_buffer_return(buf->base);
    aio_report(h->loop, id, AIO_EV_UDP_RECV, UV_ENOMEM, NULL);
    return;
  }
  ctx->buf = buf->base;
  memset(&ctx->peer, 0, sizeof(ctx->peer));
  if (addr) {
    size_t n = addr->sa_family == AF_INET6
      ? sizeof(struct sockaddr_in6) : sizeof(struct sockaddr_in);
    memcpy(&ctx->peer, addr, n);
  }
  aio_report(h->loop, id, AIO_EV_UDP_RECV, nread, ctx);
}

int aio_udp_recv_start(void *h) {
  return uv_udp_recv_start((uv_udp_t *)h, aio_udp_alloc_cb, aio_udp_recv_cb);
}

int aio_udp_recv_stop(void *h) {
  return uv_udp_recv_stop((uv_udp_t *)h);
}

void aio_udp_recv_copy(void *ctx_, void *dest, int64_t n) {
  aio_udp_recv_ctx_t *ctx = (aio_udp_recv_ctx_t *)ctx_;
  memcpy(dest, ctx->buf, (size_t)n);
}

static int64_t aio_sockaddr_name(const struct sockaddr *addr,
                                 char *buf, int64_t buflen) {
  int r;
  int64_t port;
  int family;
  if (!addr) return UV_EINVAL;
  if (addr->sa_family == AF_INET) {
    const struct sockaddr_in *a4 = (const struct sockaddr_in *)addr;
    r = uv_ip4_name(a4, buf, (size_t)buflen);
    port = ntohs(a4->sin_port);
    family = 4;
  } else if (addr->sa_family == AF_INET6) {
    const struct sockaddr_in6 *a6 = (const struct sockaddr_in6 *)addr;
    r = uv_ip6_name(a6, buf, (size_t)buflen);
    port = ntohs(a6->sin6_port);
    family = 6;
  } else {
    return UV_EINVAL;
  }
  if (r != 0) return r;
  return (int64_t)family * 65536 + port;
}

int64_t aio_udp_recv_addr(void *ctx_, char *buf, int64_t buflen) {
  aio_udp_recv_ctx_t *ctx = (aio_udp_recv_ctx_t *)ctx_;
  return aio_sockaddr_name((const struct sockaddr *)&ctx->peer, buf, buflen);
}

void aio_udp_recv_free(void *ctx_) {
  aio_udp_recv_ctx_t *ctx = (aio_udp_recv_ctx_t *)ctx_;
  if (!ctx) return;
  aio_read_buffer_return(ctx->buf);
  free(ctx);
}

static void aio_udp_send_cb(uv_udp_send_t *req, int status) {
  aio_udp_send_ctx_t *ctx = (aio_udp_send_ctx_t *)req;
  int64_t id = (int64_t)(intptr_t)uv_req_get_data((uv_req_t *)req);
  aio_report(req->handle->loop, id, AIO_EV_UDP_SEND, status, NULL);
  free(ctx->buf);
  free(ctx);
}

int aio_udp_send(void *h, const void *src, int64_t len,
                 const char *host, int64_t port, int64_t reqid) {
  aio_udp_send_ctx_t *ctx;
  uv_buf_t buf;
  struct sockaddr_storage addr;
  const struct sockaddr *dest = NULL;
  int r;
  if (len < 0 || (uint64_t)len > UINT_MAX) return UV_EINVAL;
  if (host && *host) {
    r = aio_parse_addr(host, port, &addr);
    if (r != 0) return r;
    dest = (const struct sockaddr *)&addr;
  }
  ctx = (aio_udp_send_ctx_t *)malloc(sizeof(*ctx));
  if (!ctx) return UV_ENOMEM;
  ctx->buf = (char *)malloc((size_t)len ? (size_t)len : 1);
  if (!ctx->buf) { free(ctx); return UV_ENOMEM; }
  memcpy(ctx->buf, src, (size_t)len);
  buf = uv_buf_init(ctx->buf, (unsigned int)len);
  uv_req_set_data((uv_req_t *)&ctx->req, (void *)(intptr_t)reqid);
  r = uv_udp_send(&ctx->req, (uv_udp_t *)h, &buf, 1, dest, aio_udp_send_cb);
  if (r != 0) { free(ctx->buf); free(ctx); }
  return r;
}

int64_t aio_udp_address(void *h, int peer, char *buf, int64_t buflen) {
  struct sockaddr_storage addr;
  int len = sizeof(addr);
  int r = peer
    ? uv_udp_getpeername((const uv_udp_t *)h, (struct sockaddr *)&addr, &len)
    : uv_udp_getsockname((const uv_udp_t *)h, (struct sockaddr *)&addr, &len);
  if (r != 0) return r;
  return aio_sockaddr_name((const struct sockaddr *)&addr, buf, buflen);
}

int aio_udp_set_membership(void *h, const char *multicast,
                           const char *iface, const char *source, int action) {
  const char *ifp = (iface && *iface) ? iface : NULL;
  if (source && *source)
    return uv_udp_set_source_membership((uv_udp_t *)h, multicast, ifp, source,
      action ? UV_JOIN_GROUP : UV_LEAVE_GROUP);
  return uv_udp_set_membership((uv_udp_t *)h, multicast, ifp,
    action ? UV_JOIN_GROUP : UV_LEAVE_GROUP);
}

int aio_udp_set_option(void *h, int option, int64_t value) {
  switch (option) {
    case 0: return uv_udp_set_multicast_loop((uv_udp_t *)h, (int)value);
    case 1: return uv_udp_set_multicast_ttl((uv_udp_t *)h, (int)value);
    case 3: return uv_udp_set_broadcast((uv_udp_t *)h, (int)value);
    case 4: return uv_udp_set_ttl((uv_udp_t *)h, (int)value);
    default: return UV_EINVAL;
  }
}

int aio_udp_set_multicast_interface(void *h, const char *iface) {
  return uv_udp_set_multicast_interface((uv_udp_t *)h,
    (iface && *iface) ? iface : NULL);
}

/* ------------------------------------------------------------- dns ------ */

static void aio_getaddrinfo_cb(uv_getaddrinfo_t *req, int status,
                               struct addrinfo *res) {
  aio_dns_ctx_t *ctx = (aio_dns_ctx_t *)req;
  int64_t id = (int64_t)(intptr_t)uv_req_get_data((uv_req_t *)req);
  /* results stay on the ctx (req->addrinfo == res) until aio_dns_free */
  (void)res;
  aio_report(req->loop, id, AIO_EV_DNS, status, ctx);
}

intptr_t aio_dns_lookup(void *al_, const char *node, const char *service,
                   int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_dns_ctx_t *ctx = (aio_dns_ctx_t *)malloc(sizeof(aio_dns_ctx_t));
  struct addrinfo hints;
  int r;
  if (!ctx) return UV_ENOMEM;
  ctx->node = aio_strdup(node);
  ctx->service = (service && *service) ? aio_strdup(service) : NULL;
  if (!ctx->node || ((service && *service) && !ctx->service)) {
    free(ctx->node);
    free(ctx->service);
    free(ctx);
    return UV_ENOMEM;
  }
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  uv_req_set_data((uv_req_t *)&ctx->gai, (void *)(intptr_t)reqid);
  r = uv_getaddrinfo(al->loop, &ctx->gai, aio_getaddrinfo_cb,
                     ctx->node, ctx->service, &hints);
  if (r != 0) {
    free(ctx->node);
    free(ctx->service);
    free(ctx);
    return (intptr_t)r;
  }
  return (intptr_t)ctx;
}

int aio_dns_cancel(void *ctx_) {
  aio_dns_ctx_t *ctx = (aio_dns_ctx_t *)ctx_;
  return uv_cancel((uv_req_t *)&ctx->gai);
}

int64_t aio_dns_count(void *ctx_) {
  aio_dns_ctx_t *ctx = (aio_dns_ctx_t *)ctx_;
  struct addrinfo *ai = ctx->gai.addrinfo;
  int64_t n = 0;
  while (ai) { n++; ai = ai->ai_next; }
  return n;
}

/* writes the numeric address of result i into buf; returns
   family*65536+port with family normalized to 4 or 6, or a negative error */
int64_t aio_dns_addr(void *ctx_, int64_t i, char *buf, int64_t buflen) {
  aio_dns_ctx_t *ctx = (aio_dns_ctx_t *)ctx_;
  struct addrinfo *ai = ctx->gai.addrinfo;
  int64_t port;
  int fam;
  int r;
  while (ai && i > 0) { ai = ai->ai_next; i--; }
  if (!ai) return UV_EINVAL;
  if (ai->ai_family == AF_INET) {
    struct sockaddr_in *a4 = (struct sockaddr_in *)ai->ai_addr;
    port = ntohs(a4->sin_port);
    fam = 4;
    r = uv_ip4_name(a4, buf, (size_t)buflen);
  } else if (ai->ai_family == AF_INET6) {
    struct sockaddr_in6 *a6 = (struct sockaddr_in6 *)ai->ai_addr;
    port = ntohs(a6->sin6_port);
    fam = 6;
    r = uv_ip6_name(a6, buf, (size_t)buflen);
  } else {
    return UV_EAI_FAMILY;
  }
  if (r != 0) return r;
  return (int64_t)fam * 65536 + port;
}

void aio_dns_free(void *ctx_) {
  aio_dns_ctx_t *ctx = (aio_dns_ctx_t *)ctx_;
  if (ctx->gai.addrinfo) uv_freeaddrinfo(ctx->gai.addrinfo);
  free(ctx->node);
  free(ctx->service);
  free(ctx);
}

static void aio_nameinfo_cb(uv_getnameinfo_t *req, int status,
                            const char *hostname, const char *service) {
  aio_nameinfo_ctx_t *ctx = (aio_nameinfo_ctx_t *)req;
  int64_t id = (int64_t)(intptr_t)uv_req_get_data((uv_req_t *)req);
  if (status == 0) {
    ctx->host = aio_strdup(hostname ? hostname : "");
    ctx->service = aio_strdup(service ? service : "");
    if (!ctx->host || !ctx->service) status = UV_ENOMEM;
  }
  aio_report(req->loop, id, AIO_EV_NAMEINFO, status, ctx);
}

intptr_t aio_dns_reverse(void *al_, const char *host, int64_t port,
                         int flags, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_nameinfo_ctx_t *ctx;
  struct sockaddr_storage addr;
  int r = aio_parse_addr(host, port, &addr);
  if (r != 0) return r;
  ctx = (aio_nameinfo_ctx_t *)calloc(1, sizeof(*ctx));
  if (!ctx) return UV_ENOMEM;
  uv_req_set_data((uv_req_t *)&ctx->req, (void *)(intptr_t)reqid);
  {
    int uv_flags = 0;
    if (flags & 1) uv_flags |= NI_NAMEREQD;
    if (flags & 2) uv_flags |= NI_NUMERICHOST;
    if (flags & 4) uv_flags |= NI_NUMERICSERV;
    r = uv_getnameinfo(al->loop, &ctx->req, aio_nameinfo_cb,
                       (const struct sockaddr *)&addr, uv_flags);
  }
  if (r != 0) { free(ctx); return r; }
  return (intptr_t)ctx;
}

int aio_dns_reverse_cancel(void *ctx_) {
  aio_nameinfo_ctx_t *ctx = (aio_nameinfo_ctx_t *)ctx_;
  return uv_cancel((uv_req_t *)&ctx->req);
}

int64_t aio_dns_reverse_copy(void *ctx_, int service,
                             char *buf, int64_t buflen) {
  aio_nameinfo_ctx_t *ctx = (aio_nameinfo_ctx_t *)ctx_;
  const char *s = service ? ctx->service : ctx->host;
  size_t n;
  if (!s) return UV_EINVAL;
  n = strlen(s);
  if (!buf || buflen <= 0 || n >= (size_t)buflen) return UV_ENOBUFS;
  memcpy(buf, s, n + 1);
  return (int64_t)n;
}

void aio_dns_reverse_free(void *ctx_) {
  aio_nameinfo_ctx_t *ctx = (aio_nameinfo_ctx_t *)ctx_;
  free(ctx->host);
  free(ctx->service);
  free(ctx);
}

static void aio_random_cb(uv_random_t *req, int status, void *buf,
                          size_t buflen) {
  aio_random_ctx_t *ctx = (aio_random_ctx_t *)req;
  int64_t id = (int64_t)(intptr_t)uv_req_get_data((uv_req_t *)req);
  (void)buf;
  (void)buflen;
  aio_report(req->loop, id, AIO_EV_RANDOM, status, ctx);
}

intptr_t aio_random(void *al_, int64_t len, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_random_ctx_t *ctx;
  int r;
  if (len < 0 || (uint64_t)len > SIZE_MAX) return UV_EINVAL;
  ctx = (aio_random_ctx_t *)calloc(1, sizeof(*ctx));
  if (!ctx) return UV_ENOMEM;
  ctx->len = (size_t)len;
  ctx->buf = (char *)malloc(ctx->len ? ctx->len : 1);
  if (!ctx->buf) { free(ctx); return UV_ENOMEM; }
  uv_req_set_data((uv_req_t *)&ctx->req, (void *)(intptr_t)reqid);
  r = uv_random(al->loop, &ctx->req, ctx->buf, ctx->len, 0, aio_random_cb);
  if (r != 0) { free(ctx->buf); free(ctx); return r; }
  return (intptr_t)ctx;
}

int aio_random_cancel(void *ctx_) {
  aio_random_ctx_t *ctx = (aio_random_ctx_t *)ctx_;
  return uv_cancel((uv_req_t *)&ctx->req);
}

void aio_random_copy(void *ctx_, void *dest) {
  aio_random_ctx_t *ctx = (aio_random_ctx_t *)ctx_;
  memcpy(dest, ctx->buf, ctx->len);
}

void aio_random_free(void *ctx_) {
  aio_random_ctx_t *ctx = (aio_random_ctx_t *)ctx_;
  free(ctx->buf);
  free(ctx);
}

static void aio_poll_cb(uv_poll_t *h, int status, int events) {
  int64_t id = (int64_t)(intptr_t)uv_handle_get_data((uv_handle_t *)h);
  uv_poll_stop(h);
  aio_report(h->loop, id, AIO_EV_POLL, status < 0 ? status : events, NULL);
}

void *aio_poll_init(void *al_, int64_t fd, int64_t id) {
  aio_loop_t *al = (aio_loop_t *)al_;
  uv_poll_t *h = (uv_poll_t *)malloc(sizeof(*h));
  int r;
  if (!h) return NULL;
  r = uv_poll_init(al->loop, h, (int)fd);
  if (r != 0) { free(h); return NULL; }
  uv_handle_set_data((uv_handle_t *)h, (void *)(intptr_t)id);
  return h;
}

int aio_poll_start(void *h, int events) {
  int uv_events = 0;
  if (events & 1) uv_events |= UV_READABLE;
  if (events & 2) uv_events |= UV_WRITABLE;
  if (events & 4) uv_events |= UV_DISCONNECT;
  if (events & 8) uv_events |= UV_PRIORITIZED;
  return uv_poll_start((uv_poll_t *)h, uv_events, aio_poll_cb);
}

int aio_poll_stop(void *h) { return uv_poll_stop((uv_poll_t *)h); }

static char **aio_blob_strings(const char *blob, int64_t len) {
  char **strings;
  int64_t i;
  size_t count = 0;
  if (!blob || len <= 0) return NULL;
  if (blob[len - 1] != '\0') return NULL;
  if (len == 1 && blob[0] == '\0')
    return (char **)calloc(1, sizeof(char *));
  for (i = 0; i < len; i++) if (blob[i] == '\0') count++;
  strings = (char **)calloc(count + 1, sizeof(char *));
  if (!strings) return NULL;
  strings[0] = (char *)blob;
  count = 1;
  for (i = 0; i + 1 < len; i++)
    if (blob[i] == '\0') strings[count++] = (char *)blob + i + 1;
  strings[count] = NULL;
  return strings;
}

static void aio_process_exit_cb(uv_process_t *process, int64_t exit_status,
                                int term_signal) {
  int64_t id = (int64_t)(intptr_t)uv_handle_get_data((uv_handle_t *)process);
  aio_process_exit_t *result =
      (aio_process_exit_t *)malloc(sizeof(*result));
  if (result) result->term_signal = term_signal;
  aio_report(process->loop, id, AIO_EV_PROCESS, exit_status, result);
}

intptr_t aio_process_spawn(void *al_, int64_t id, const char *file,
                           const char *args_blob, int64_t args_len,
                           const char *env_blob, int64_t env_len,
                           int env_present, const char *cwd, int flags,
                           int in_mode, int64_t in_fd, void *in_pipe,
                           int out_mode, int64_t out_fd, void *out_pipe,
                           int err_mode, int64_t err_fd, void *err_pipe) {
  aio_loop_t *al = (aio_loop_t *)al_;
  uv_process_t *process;
  uv_process_options_t options;
  uv_stdio_container_t stdio[3];
  char **args = aio_blob_strings(args_blob, args_len);
  char **env = env_present ? aio_blob_strings(env_blob, env_len) : NULL;
  int modes[3] = {in_mode, out_mode, err_mode};
  int64_t fds[3] = {in_fd, out_fd, err_fd};
  void *pipes[3] = {in_pipe, out_pipe, err_pipe};
  int i;
  int r;
  if (!args || (env_present && !env)) {
    free(args);
    free(env);
    return UV_EINVAL;
  }
  process = (uv_process_t *)malloc(sizeof(*process));
  if (!process) { free(args); free(env); return UV_ENOMEM; }
  memset(&options, 0, sizeof(options));
  memset(stdio, 0, sizeof(stdio));
  options.exit_cb = aio_process_exit_cb;
  options.file = file;
  options.args = args;
  options.env = env;
  options.cwd = (cwd && *cwd) ? cwd : NULL;
  if (flags & 1) options.flags |= UV_PROCESS_DETACHED;
  if (flags & 2) options.flags |= UV_PROCESS_WINDOWS_HIDE;
  if (flags & 4) options.flags |= UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS;
  for (i = 0; i < 3; i++) {
    switch (modes[i]) {
      case 0: stdio[i].flags = UV_IGNORE; break;
      case 1:
        stdio[i].flags = UV_INHERIT_FD;
        stdio[i].data.fd = (int)fds[i];
        break;
      case 2:
        stdio[i].flags = UV_CREATE_PIPE | UV_READABLE_PIPE;
        stdio[i].data.stream = (uv_stream_t *)pipes[i];
        break;
      case 3:
        stdio[i].flags = UV_CREATE_PIPE | UV_WRITABLE_PIPE;
        stdio[i].data.stream = (uv_stream_t *)pipes[i];
        break;
      default:
        free(process); free(args); free(env); return UV_EINVAL;
    }
  }
  options.stdio_count = 3;
  options.stdio = stdio;
  r = uv_spawn(al->loop, process, &options);
  free(args);
  free(env);
  if (r != 0) { free(process); return r; }
  uv_handle_set_data((uv_handle_t *)process, (void *)(intptr_t)id);
  return (intptr_t)process;
}

int64_t aio_process_pid(void *process) {
  return (int64_t)uv_process_get_pid((const uv_process_t *)process);
}

int aio_process_kill(void *process, int signum) {
  return uv_process_kill((uv_process_t *)process, signum);
}

int aio_kill(int64_t pid, int signum) { return uv_kill((int)pid, signum); }

int64_t aio_process_term_signal(void *result_) {
  aio_process_exit_t *result = (aio_process_exit_t *)result_;
  return result ? result->term_signal : 0;
}

void aio_process_result_free(void *result) { free(result); }

static void aio_signal_cb(uv_signal_t *h, int signum) {
  int64_t id = (int64_t)(intptr_t)uv_handle_get_data((uv_handle_t *)h);
  uv_signal_stop(h);
  aio_report(h->loop, id, AIO_EV_SIGNAL, signum, NULL);
}

void *aio_signal_init(void *al_, int64_t id) {
  aio_loop_t *al = (aio_loop_t *)al_;
  uv_signal_t *h = (uv_signal_t *)malloc(sizeof(*h));
  if (!h) return NULL;
  if (uv_signal_init(al->loop, h) != 0) { free(h); return NULL; }
  uv_handle_set_data((uv_handle_t *)h, (void *)(intptr_t)id);
  return h;
}

int aio_signal_start(void *h, int signum, int oneshot) {
  return oneshot
    ? uv_signal_start_oneshot((uv_signal_t *)h, aio_signal_cb, signum)
    : uv_signal_start((uv_signal_t *)h, aio_signal_cb, signum);
}

int aio_signal_stop(void *h) { return uv_signal_stop((uv_signal_t *)h); }

static void aio_fs_event_cb(uv_fs_event_t *h, const char *filename,
                            int events, int status) {
  int64_t id = (int64_t)(intptr_t)uv_handle_get_data((uv_handle_t *)h);
  aio_fs_event_result_t *result = NULL;
  uv_fs_event_stop(h);
  if (status == 0) {
    result = (aio_fs_event_result_t *)calloc(1, sizeof(*result));
    if (!result) status = UV_ENOMEM;
    else {
      result->filename = filename ? aio_strdup(filename) : aio_strdup("");
      result->events = events;
      if (!result->filename) { free(result); result = NULL; status = UV_ENOMEM; }
    }
  }
  aio_report(h->loop, id, AIO_EV_FS_EVENT, status, result);
}

void *aio_fs_event_init(void *al_, int64_t id) {
  aio_loop_t *al = (aio_loop_t *)al_;
  uv_fs_event_t *h = (uv_fs_event_t *)malloc(sizeof(*h));
  if (!h) return NULL;
  if (uv_fs_event_init(al->loop, h) != 0) { free(h); return NULL; }
  uv_handle_set_data((uv_handle_t *)h, (void *)(intptr_t)id);
  return h;
}

int aio_fs_event_start(void *h, const char *path, int flags) {
  unsigned int uv_flags = 0;
  if (flags & 1) uv_flags |= UV_FS_EVENT_WATCH_ENTRY;
  if (flags & 2) uv_flags |= UV_FS_EVENT_STAT;
  if (flags & 4) uv_flags |= UV_FS_EVENT_RECURSIVE;
  return uv_fs_event_start((uv_fs_event_t *)h, aio_fs_event_cb, path, uv_flags);
}

int aio_fs_event_stop(void *h) { return uv_fs_event_stop((uv_fs_event_t *)h); }

int aio_fs_event_result_copy(void *result_, char *buf, int64_t buflen) {
  aio_fs_event_result_t *result = (aio_fs_event_result_t *)result_;
  size_t n;
  if (!result || !result->filename) return UV_EINVAL;
  n = strlen(result->filename);
  if (!buf || buflen <= 0 || n >= (size_t)buflen) return UV_ENOBUFS;
  memcpy(buf, result->filename, n + 1);
  return result->events;
}

void aio_fs_event_result_free(void *result_) {
  aio_fs_event_result_t *result = (aio_fs_event_result_t *)result_;
  if (!result) return;
  free(result->filename);
  free(result);
}

static void aio_fs_poll_cb(uv_fs_poll_t *h, int status,
                           const uv_stat_t *previous,
                           const uv_stat_t *current) {
  int64_t id = (int64_t)(intptr_t)uv_handle_get_data((uv_handle_t *)h);
  aio_fs_poll_result_t *result = NULL;
  uv_fs_poll_stop(h);
  if (status == 0) {
    result = (aio_fs_poll_result_t *)malloc(sizeof(*result));
    if (!result) status = UV_ENOMEM;
    else { result->previous = *previous; result->current = *current; }
  }
  aio_report(h->loop, id, AIO_EV_FS_POLL, status, result);
}

void *aio_fs_poll_init(void *al_, int64_t id) {
  aio_loop_t *al = (aio_loop_t *)al_;
  uv_fs_poll_t *h = (uv_fs_poll_t *)malloc(sizeof(*h));
  if (!h) return NULL;
  if (uv_fs_poll_init(al->loop, h) != 0) { free(h); return NULL; }
  uv_handle_set_data((uv_handle_t *)h, (void *)(intptr_t)id);
  return h;
}

int aio_fs_poll_start(void *h, const char *path, int64_t interval) {
  if (interval < 1) return UV_EINVAL;
  return uv_fs_poll_start((uv_fs_poll_t *)h, aio_fs_poll_cb, path,
                          (unsigned int)interval);
}

int aio_fs_poll_stop(void *h) { return uv_fs_poll_stop((uv_fs_poll_t *)h); }

static int64_t aio_stat_value(const uv_stat_t *st, int64_t index) {
  switch (index) {
    case 0: return (int64_t)st->st_dev;
    case 1: return (int64_t)st->st_mode;
    case 2: return (int64_t)st->st_nlink;
    case 3: return (int64_t)st->st_uid;
    case 4: return (int64_t)st->st_gid;
    case 5: return (int64_t)st->st_rdev;
    case 6: return (int64_t)st->st_ino;
    case 7: return (int64_t)st->st_size;
    case 8: return (int64_t)st->st_blksize;
    case 9: return (int64_t)st->st_blocks;
    case 10: return (int64_t)st->st_flags;
    case 11: return (int64_t)st->st_gen;
    case 12: return (int64_t)st->st_atim.tv_sec;
    case 13: return (int64_t)st->st_atim.tv_nsec;
    case 14: return (int64_t)st->st_mtim.tv_sec;
    case 15: return (int64_t)st->st_mtim.tv_nsec;
    case 16: return (int64_t)st->st_ctim.tv_sec;
    case 17: return (int64_t)st->st_ctim.tv_nsec;
    default: return 0;
  }
}

int64_t aio_fs_poll_result_field(void *result_, int current, int64_t index) {
  aio_fs_poll_result_t *result = (aio_fs_poll_result_t *)result_;
  return aio_stat_value(current ? &result->current : &result->previous, index);
}

void aio_fs_poll_result_free(void *result) { free(result); }

void *aio_tty_init(void *al_, int64_t fd, int64_t id) {
  aio_loop_t *al = (aio_loop_t *)al_;
  uv_tty_t *h = (uv_tty_t *)malloc(sizeof(*h));
  if (!h) return NULL;
  if (uv_tty_init(al->loop, h, (uv_file)fd, 0) != 0) {
    free(h);
    return NULL;
  }
  uv_handle_set_data((uv_handle_t *)h, (void *)(intptr_t)id);
  return h;
}

int aio_tty_set_mode(void *h, int mode) {
  uv_tty_mode_t uv_mode;
  switch (mode) {
    case 0: uv_mode = UV_TTY_MODE_NORMAL; break;
    case 1: uv_mode = UV_TTY_MODE_RAW; break;
    case 2: uv_mode = UV_TTY_MODE_IO; break;
    default: return UV_EINVAL;
  }
  return uv_tty_set_mode((uv_tty_t *)h, uv_mode);
}

int aio_tty_get_winsize(void *h, int *width, int *height) {
  return uv_tty_get_winsize((uv_tty_t *)h, width, height);
}

int aio_tty_winsize(void *h, int dimension) {
  int width;
  int height;
  int r = uv_tty_get_winsize((uv_tty_t *)h, &width, &height);
  if (r != 0) return r;
  return dimension ? height : width;
}

int aio_tty_get_vterm_state(void) {
  uv_tty_vtermstate_t state;
  int r = uv_tty_get_vterm_state(&state);
  return r == 0 ? (int)state : r;
}

void aio_tty_set_vterm_state(int state) {
  uv_tty_set_vterm_state(state ? UV_TTY_SUPPORTED : UV_TTY_UNSUPPORTED);
}

void aio_tty_reset_mode(void) { uv_tty_reset_mode(); }

typedef struct {
  uv_cpu_info_t *items;
  int count;
} aio_cpu_info_ctx_t;

typedef struct {
  uv_interface_address_t *items;
  int count;
} aio_interface_ctx_t;

typedef struct { uv_rusage_t value; } aio_rusage_ctx_t;

uint64_t aio_system_u64(int field) {
  size_t rss = 0;
  switch (field) {
    case 0: return uv_hrtime();
    case 1: return uv_get_total_memory();
    case 2: return uv_get_free_memory();
    case 3: return uv_get_constrained_memory();
    case 4: return uv_get_available_memory();
    case 5: return uv_resident_set_memory(&rss) == 0 ? (uint64_t)rss : 0;
    case 6: return (uint64_t)uv_os_getpid();
    case 7: return (uint64_t)uv_os_getppid();
    case 8: return (uint64_t)uv_available_parallelism();
    default: return 0;
  }
}

double aio_system_double(int field) {
  double values[3];
  double uptime = 0;
  if (field == 0) {
    return uv_uptime(&uptime) == 0 ? uptime : -1.0;
  }
  uv_loadavg(values);
  return (field >= 1 && field <= 3) ? values[field - 1] : -1.0;
}

int aio_system_string(int field, char *buf, int64_t buflen) {
  size_t size;
  if (!buf || buflen <= 0) return UV_EINVAL;
  size = (size_t)buflen;
  switch (field) {
    case 0: return uv_exepath(buf, &size);
    case 1: return uv_cwd(buf, &size);
    case 2: return uv_os_homedir(buf, &size);
    case 3: return uv_os_tmpdir(buf, &size);
    case 4: return uv_os_gethostname(buf, &size);
    default: return UV_EINVAL;
  }
}

int aio_uname_string(int field, char *buf, int64_t buflen) {
  uv_utsname_t u;
  const char *s;
  size_t n;
  int r = uv_os_uname(&u);
  if (r != 0) return r;
  switch (field) {
    case 0: s = u.sysname; break;
    case 1: s = u.release; break;
    case 2: s = u.version; break;
    case 3: s = u.machine; break;
    default: return UV_EINVAL;
  }
  n = strlen(s);
  if (!buf || buflen <= 0 || n >= (size_t)buflen) return UV_ENOBUFS;
  memcpy(buf, s, n + 1);
  return 0;
}

void *aio_cpu_info(void) {
  aio_cpu_info_ctx_t *ctx = (aio_cpu_info_ctx_t *)calloc(1, sizeof(*ctx));
  if (!ctx) return NULL;
  if (uv_cpu_info(&ctx->items, &ctx->count) != 0) { free(ctx); return NULL; }
  return ctx;
}

int64_t aio_cpu_info_count(void *ctx_) {
  return ((aio_cpu_info_ctx_t *)ctx_)->count;
}

int aio_cpu_info_model(void *ctx_, int64_t index, char *buf, int64_t buflen) {
  aio_cpu_info_ctx_t *ctx = (aio_cpu_info_ctx_t *)ctx_;
  size_t n;
  if (index < 0 || index >= ctx->count) return UV_EINVAL;
  n = strlen(ctx->items[index].model);
  if (!buf || buflen <= 0 || n >= (size_t)buflen) return UV_ENOBUFS;
  memcpy(buf, ctx->items[index].model, n + 1);
  return 0;
}

uint64_t aio_cpu_info_field(void *ctx_, int64_t index, int field) {
  aio_cpu_info_ctx_t *ctx = (aio_cpu_info_ctx_t *)ctx_;
  uv_cpu_info_t *cpu;
  if (index < 0 || index >= ctx->count) return 0;
  cpu = &ctx->items[index];
  switch (field) {
    case 0: return (uint64_t)cpu->speed;
    case 1: return cpu->cpu_times.user;
    case 2: return cpu->cpu_times.nice;
    case 3: return cpu->cpu_times.sys;
    case 4: return cpu->cpu_times.idle;
    case 5: return cpu->cpu_times.irq;
    default: return 0;
  }
}

void aio_cpu_info_free(void *ctx_) {
  aio_cpu_info_ctx_t *ctx = (aio_cpu_info_ctx_t *)ctx_;
  uv_free_cpu_info(ctx->items, ctx->count);
  free(ctx);
}

void *aio_interface_info(void) {
  aio_interface_ctx_t *ctx = (aio_interface_ctx_t *)calloc(1, sizeof(*ctx));
  if (!ctx) return NULL;
  if (uv_interface_addresses(&ctx->items, &ctx->count) != 0) {
    free(ctx); return NULL;
  }
  return ctx;
}

int64_t aio_interface_count(void *ctx_) {
  return ((aio_interface_ctx_t *)ctx_)->count;
}

int aio_interface_name(void *ctx_, int64_t index, char *buf, int64_t buflen) {
  aio_interface_ctx_t *ctx = (aio_interface_ctx_t *)ctx_;
  size_t n;
  if (index < 0 || index >= ctx->count) return UV_EINVAL;
  n = strlen(ctx->items[index].name);
  if (!buf || buflen <= 0 || n >= (size_t)buflen) return UV_ENOBUFS;
  memcpy(buf, ctx->items[index].name, n + 1);
  return 0;
}

int64_t aio_interface_address(void *ctx_, int64_t index, int netmask,
                              char *buf, int64_t buflen) {
  aio_interface_ctx_t *ctx = (aio_interface_ctx_t *)ctx_;
  uv_interface_address_t *item;
  const struct sockaddr *addr;
  if (index < 0 || index >= ctx->count) return UV_EINVAL;
  item = &ctx->items[index];
  addr = netmask ? (const struct sockaddr *)&item->netmask
                 : (const struct sockaddr *)&item->address;
  return aio_sockaddr_name(addr, buf, buflen);
}

int aio_interface_internal(void *ctx_, int64_t index) {
  aio_interface_ctx_t *ctx = (aio_interface_ctx_t *)ctx_;
  return (index >= 0 && index < ctx->count) ? ctx->items[index].is_internal : 0;
}

int aio_interface_physical(void *ctx_, int64_t index, void *buf) {
  aio_interface_ctx_t *ctx = (aio_interface_ctx_t *)ctx_;
  if (index < 0 || index >= ctx->count) return UV_EINVAL;
  memcpy(buf, ctx->items[index].phys_addr, sizeof(ctx->items[index].phys_addr));
  return (int)sizeof(ctx->items[index].phys_addr);
}

void aio_interface_free(void *ctx_) {
  aio_interface_ctx_t *ctx = (aio_interface_ctx_t *)ctx_;
  uv_free_interface_addresses(ctx->items, ctx->count);
  free(ctx);
}

void *aio_rusage(void) {
  aio_rusage_ctx_t *ctx = (aio_rusage_ctx_t *)malloc(sizeof(*ctx));
  if (!ctx) return NULL;
  if (uv_getrusage(&ctx->value) != 0) { free(ctx); return NULL; }
  return ctx;
}

int64_t aio_rusage_field(void *ctx_, int field) {
  uv_rusage_t *r = &((aio_rusage_ctx_t *)ctx_)->value;
  switch (field) {
    case 0: return r->ru_utime.tv_sec;
    case 1: return r->ru_utime.tv_usec;
    case 2: return r->ru_stime.tv_sec;
    case 3: return r->ru_stime.tv_usec;
    case 4: return r->ru_maxrss;
    case 5: return r->ru_ixrss;
    case 6: return r->ru_idrss;
    case 7: return r->ru_isrss;
    case 8: return r->ru_minflt;
    case 9: return r->ru_majflt;
    case 10: return r->ru_nswap;
    case 11: return r->ru_inblock;
    case 12: return r->ru_oublock;
    case 13: return r->ru_msgsnd;
    case 14: return r->ru_msgrcv;
    case 15: return r->ru_nsignals;
    case 16: return r->ru_nvcsw;
    case 17: return r->ru_nivcsw;
    default: return 0;
  }
}

void aio_rusage_free(void *ctx) { free(ctx); }

int64_t aio_loop_metric(void *al_, int field) {
  aio_loop_t *al = (aio_loop_t *)al_;
  uv_metrics_t metrics;
  switch (field) {
    case 0: return (int64_t)uv_now(al->loop);
    case 1: return (int64_t)uv_metrics_idle_time(al->loop);
    case 2: return (int64_t)uv_backend_timeout(al->loop);
    case 3: return (int64_t)uv_backend_fd(al->loop);
    case 4: return uv_loop_alive(al->loop) ? 1 : 0;
    default:
      if (uv_metrics_info(al->loop, &metrics) != 0) return 0;
      if (field == 5) return (int64_t)metrics.loop_count;
      if (field == 6) return (int64_t)metrics.events;
      if (field == 7) return (int64_t)metrics.events_waiting;
      return 0;
  }
}

/* -------------------------------------------------------------- fs ------ */

static void aio_fs_cb(uv_fs_t *req) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)req;
  int64_t id = (int64_t)(intptr_t)uv_req_get_data((uv_req_t *)req);
#if defined(_WIN32)
  if (ctx->temp_fd1 >= 0) _close(ctx->temp_fd1);
  if (ctx->temp_fd2 >= 0) _close(ctx->temp_fd2);
#else
  if (ctx->temp_fd1 >= 0) close(ctx->temp_fd1);
  if (ctx->temp_fd2 >= 0) close(ctx->temp_fd2);
#endif
  ctx->temp_fd1 = ctx->temp_fd2 = -1;
  if (ctx->dir) {
    ctx->dir->dirents = NULL;
    ctx->dir->nentries = 0;
    ctx->dir = NULL;
  }
  aio_report(req->loop, id, AIO_EV_FS, uv_fs_get_result(req), ctx);
}

static aio_fs_ctx_t *aio_fs_ctx_new(int64_t reqid) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)malloc(sizeof(aio_fs_ctx_t));
  if (ctx) {
    memset(ctx, 0, sizeof(*ctx));
    ctx->buf = NULL;
    ctx->dirents = NULL;
    ctx->dirent_count = 0;
    ctx->dir = NULL;
    ctx->temp_fd1 = -1;
    ctx->temp_fd2 = -1;
    uv_req_set_data((uv_req_t *)&ctx->fs, (void *)(intptr_t)reqid);
  }
  return ctx;
}

static intptr_t aio_fs_submit_failed(aio_fs_ctx_t *ctx, int r) {
  uv_fs_req_cleanup(&ctx->fs);
  if (ctx->dir) {
    ctx->dir->dirents = NULL;
    ctx->dir->nentries = 0;
  }
#if defined(_WIN32)
  if (ctx->temp_fd1 >= 0) _close(ctx->temp_fd1);
  if (ctx->temp_fd2 >= 0) _close(ctx->temp_fd2);
#else
  if (ctx->temp_fd1 >= 0) close(ctx->temp_fd1);
  if (ctx->temp_fd2 >= 0) close(ctx->temp_fd2);
#endif
  free(ctx->buf);
  free(ctx->dirents);
  free(ctx);
  return (intptr_t)r;
}

void aio_fs_req_free(void *ctx_) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)ctx_;
  uv_fs_req_cleanup(&ctx->fs);
  free(ctx->dirents);
  free(ctx);
}

int aio_fs_cancel(void *ctx_) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)ctx_;
  return uv_cancel((uv_req_t *)&ctx->fs);
}

/* data pointer valid for read/stat results until aio_fs_req_free */
void *aio_fs_data(void *ctx_) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)ctx_;
  return ctx->buf ? (void *)ctx->buf : ctx->fs.ptr;
}

/* stat fields by fixed index: 0 dev, 1 mode, 2 nlink, 3 uid, 4 gid,
   5 rdev, 6 ino, 7 size, 8 blksize, 9 blocks, 10 flags, 11 gen,
   12 atim-s, 13 atim-ns, 14 mtim-s, 15 mtim-ns, 16 ctim-s, 17 ctim-ns */
int64_t aio_fs_stat_field(void *ctx_, int64_t index) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)ctx_;
  const uv_stat_t *st = uv_fs_get_statbuf(&ctx->fs);
  switch (index) {
    case 0: return (int64_t)st->st_dev;
    case 1: return (int64_t)st->st_mode;
    case 2: return (int64_t)st->st_nlink;
    case 3: return (int64_t)st->st_uid;
    case 4: return (int64_t)st->st_gid;
    case 5: return (int64_t)st->st_rdev;
    case 6: return (int64_t)st->st_ino;
    case 7: return (int64_t)st->st_size;
    case 8: return (int64_t)st->st_blksize;
    case 9: return (int64_t)st->st_blocks;
    case 10: return (int64_t)st->st_flags;
    case 11: return (int64_t)st->st_gen;
    case 12: return (int64_t)st->st_atim.tv_sec;
    case 13: return (int64_t)st->st_atim.tv_nsec;
    case 14: return (int64_t)st->st_mtim.tv_sec;
    case 15: return (int64_t)st->st_mtim.tv_nsec;
    case 16: return (int64_t)st->st_ctim.tv_sec;
    case 17: return (int64_t)st->st_ctim.tv_nsec;
    default: return 0;
  }
}

/* portable open flags: 1 read, 2 write, 4 create, 8 truncate, 16 append,
   32 exclusive; translated here because O_* values are platform-specific */
static int aio_map_open_flags(int f) {
  int r;
  if ((f & 1) && (f & 2)) r = O_RDWR;
  else if (f & 2) r = O_WRONLY;
  else r = O_RDONLY;
  if (f & 4) r |= O_CREAT;
  if (f & 8) r |= O_TRUNC;
  if (f & 16) r |= O_APPEND;
  if (f & 32) r |= O_EXCL;
  return r;
}

intptr_t aio_fs_open(void *al_, const char *path, int flags, int64_t mode,
                int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  if (!ctx) return UV_ENOMEM;
  int r = uv_fs_open(al->loop, &ctx->fs, path, aio_map_open_flags(flags),
                     (int)mode, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

/* fs_read into a C buffer; data available via aio_fs_data on completion */
intptr_t aio_fs_read(void *al_, int64_t fd, int64_t len, int64_t offset,
                int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx;
  uv_buf_t buf;
  int r;
  if (len < 0 || (uint64_t)len > UINT_MAX) return UV_EINVAL;
  ctx = aio_fs_ctx_new(reqid);
  if (!ctx) return UV_ENOMEM;
  ctx->buf = (char *)malloc((size_t)len ? (size_t)len : 1);
  if (!ctx->buf) { free(ctx); return UV_ENOMEM; }
  buf.base = ctx->buf;
  buf.len = (unsigned int)len;
  r = uv_fs_read(al->loop, &ctx->fs, (uv_file)fd, &buf, 1, offset,
                 aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_write(void *al_, int64_t fd, const void *src, int64_t len,
                 int64_t offset, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx;
  uv_buf_t buf;
  int r;
  if (len < 0 || (uint64_t)len > UINT_MAX) return UV_EINVAL;
  ctx = aio_fs_ctx_new(reqid);
  if (!ctx) return UV_ENOMEM;
  ctx->buf = (char *)malloc((size_t)len ? (size_t)len : 1);
  if (!ctx->buf) { free(ctx); return UV_ENOMEM; }
  memcpy(ctx->buf, src, (size_t)len);
  buf = uv_buf_init(ctx->buf, (unsigned int)len);
  r = uv_fs_write(al->loop, &ctx->fs, (uv_file)fd, &buf, 1, offset,
                  aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

/* free a read/write buffer after completion; the ctx itself is reclaimed
   through aio_fs_req_free */
void aio_fs_buf_free(void *ctx_) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)ctx_;
  free(ctx->buf);
  ctx->buf = NULL;
}

intptr_t aio_fs_close_fd(void *al_, int64_t fd, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  if (!ctx) return UV_ENOMEM;
  int r = uv_fs_close(al->loop, &ctx->fs, (uv_file)fd, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

/* Reclaim a descriptor returned by an open request whose Scheme waiter was
   canceled after the operating-system operation had already succeeded. */
int aio_fs_close_now(int64_t fd) {
  uv_fs_t req;
  int r;
  memset(&req, 0, sizeof(req));
  r = uv_fs_close(NULL, &req, (uv_file)fd, NULL);
  uv_fs_req_cleanup(&req);
  return r;
}

intptr_t aio_fs_stat(void *al_, const char *path, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  if (!ctx) return UV_ENOMEM;
  int r = uv_fs_stat(al->loop, &ctx->fs, path, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_fstat(void *al_, int64_t fd, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  if (!ctx) return UV_ENOMEM;
  int r = uv_fs_fstat(al->loop, &ctx->fs, (uv_file)fd, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_unlink(void *al_, const char *path, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  if (!ctx) return UV_ENOMEM;
  int r = uv_fs_unlink(al->loop, &ctx->fs, path, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_rename(void *al_, const char *old_path, const char *new_path,
                  int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  if (!ctx) return UV_ENOMEM;
  int r = uv_fs_rename(al->loop, &ctx->fs, old_path, new_path, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_mkdir(void *al_, const char *path, int64_t mode, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  if (!ctx) return UV_ENOMEM;
  int r = uv_fs_mkdir(al->loop, &ctx->fs, path, (int)mode, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_rmdir(void *al_, const char *path, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  if (!ctx) return UV_ENOMEM;
  int r = uv_fs_rmdir(al->loop, &ctx->fs, path, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_copyfile(void *al_, const char *from, const char *to,
                         int flags, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int uv_flags = 0;
  int r;
  if (!ctx) return UV_ENOMEM;
  if (flags & 1) uv_flags |= UV_FS_COPYFILE_EXCL;
  if (flags & 2) uv_flags |= UV_FS_COPYFILE_FICLONE;
  if (flags & 4) uv_flags |= UV_FS_COPYFILE_FICLONE_FORCE;
  r = uv_fs_copyfile(al->loop, &ctx->fs, from, to, uv_flags, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_mkdtemp(void *al_, const char *pattern, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = uv_fs_mkdtemp(al->loop, &ctx->fs, pattern, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_mkstemp(void *al_, const char *pattern, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = uv_fs_mkstemp(al->loop, &ctx->fs, pattern, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_scandir(void *al_, const char *path, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = uv_fs_scandir(al->loop, &ctx->fs, path, 0, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

/* Returns dirent type + 1, zero at end, or a negative libuv error. */
int aio_fs_scandir_next(void *ctx_, char *buf, int64_t buflen) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)ctx_;
  size_t n;
  int r = uv_fs_scandir_next(&ctx->fs, &ctx->dirent);
  if (r == UV_EOF) return 0;
  if (r < 0) return r;
  n = strlen(ctx->dirent.name);
  if (!buf || buflen <= 0 || n >= (size_t)buflen) return UV_ENOBUFS;
  memcpy(buf, ctx->dirent.name, n + 1);
  return (int)ctx->dirent.type + 1;
}

intptr_t aio_fs_opendir(void *al_, const char *path, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = uv_fs_opendir(al->loop, &ctx->fs, path, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_readdir(void *al_, void *dir_, int64_t count,
                         int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  uv_dir_t *dir = (uv_dir_t *)dir_;
  int r;
  if (!ctx) return UV_ENOMEM;
  if (!dir || count <= 0 || (uint64_t)count > SIZE_MAX / sizeof(uv_dirent_t))
    return aio_fs_submit_failed(ctx, UV_EINVAL);
  ctx->dirents = (uv_dirent_t *)calloc((size_t)count, sizeof(uv_dirent_t));
  if (!ctx->dirents) return aio_fs_submit_failed(ctx, UV_ENOMEM);
  ctx->dirent_count = (size_t)count;
  ctx->dir = dir;
  dir->dirents = ctx->dirents;
  dir->nentries = (size_t)count;
  r = uv_fs_readdir(al->loop, &ctx->fs, dir, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_closedir(void *al_, void *dir_, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  if (!dir_) return aio_fs_submit_failed(ctx, UV_EINVAL);
  r = uv_fs_closedir(al->loop, &ctx->fs, (uv_dir_t *)dir_, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

int aio_fs_closedir_now(void *dir_) {
  uv_fs_t req;
  int r;
  if (!dir_) return UV_EINVAL;
  memset(&req, 0, sizeof(req));
  r = uv_fs_closedir(NULL, &req, (uv_dir_t *)dir_, NULL);
  uv_fs_req_cleanup(&req);
  return r;
}

void *aio_fs_result_ptr(void *ctx_) {
  return uv_fs_get_ptr(&((aio_fs_ctx_t *)ctx_)->fs);
}

int aio_fs_readdir_entry(void *ctx_, int64_t index, char *buf,
                          int64_t buflen) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)ctx_;
  uv_dirent_t *entry;
  size_t n;
  ssize_t result = uv_fs_get_result(&ctx->fs);
  if (index < 0 || index >= result || (uint64_t)index >= ctx->dirent_count)
    return UV_EINVAL;
  entry = &ctx->dirents[index];
  n = strlen(entry->name);
  if (!buf || buflen <= 0 || n >= (size_t)buflen) return UV_ENOBUFS;
  memcpy(buf, entry->name, n + 1);
  return (int)entry->type + 1;
}

intptr_t aio_fs_fsync(void *al_, int64_t fd, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = uv_fs_fsync(al->loop, &ctx->fs, (uv_file)fd, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_fdatasync(void *al_, int64_t fd, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = uv_fs_fdatasync(al->loop, &ctx->fs, (uv_file)fd, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_ftruncate(void *al_, int64_t fd, int64_t offset,
                          int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = uv_fs_ftruncate(al->loop, &ctx->fs, (uv_file)fd, offset, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_sendfile(void *al_, int64_t out_fd, int64_t in_fd,
                         int64_t offset, int64_t length, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  uv_file send_out = (uv_file)out_fd;
  uv_file send_in = (uv_file)in_fd;
  if (!ctx) return UV_ENOMEM;
  if (length < 0 || (uint64_t)length > SIZE_MAX)
    return aio_fs_submit_failed(ctx, UV_EINVAL);
#if defined(_WIN32)
  ctx->temp_fd1 = _dup((int)out_fd);
  ctx->temp_fd2 = _dup((int)in_fd);
#else
  ctx->temp_fd1 = dup((int)out_fd);
  ctx->temp_fd2 = dup((int)in_fd);
#endif
  if (ctx->temp_fd1 < 0 || ctx->temp_fd2 < 0)
    return aio_fs_submit_failed(ctx, UV_EBADF);
  send_out = (uv_file)ctx->temp_fd1;
  send_in = (uv_file)ctx->temp_fd2;
  r = uv_fs_sendfile(al->loop, &ctx->fs, send_out, send_in, offset,
                     (size_t)length, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_access(void *al_, const char *path, int mode,
                       int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  {
    int native_mode = 0;
    if (mode & 1) native_mode |= R_OK;
    if (mode & 2) native_mode |= W_OK;
    if (mode & 4) native_mode |= X_OK;
    r = uv_fs_access(al->loop, &ctx->fs, path, native_mode, aio_fs_cb);
  }
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_chmod(void *al_, const char *path, int64_t mode,
                      int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = uv_fs_chmod(al->loop, &ctx->fs, path, (int)mode, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_fchmod(void *al_, int64_t fd, int64_t mode,
                       int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = uv_fs_fchmod(al->loop, &ctx->fs, (uv_file)fd, (int)mode, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_utime(void *al_, const char *path, double atime, double mtime,
                      int follow, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = follow
    ? uv_fs_utime(al->loop, &ctx->fs, path, atime, mtime, aio_fs_cb)
    : uv_fs_lutime(al->loop, &ctx->fs, path, atime, mtime, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_futime(void *al_, int64_t fd, double atime, double mtime,
                       int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = uv_fs_futime(al->loop, &ctx->fs, (uv_file)fd, atime, mtime, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_lstat(void *al_, const char *path, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = uv_fs_lstat(al->loop, &ctx->fs, path, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_link(void *al_, const char *from, const char *to,
                     int symbolic, int flags, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = symbolic
    ? uv_fs_symlink(al->loop, &ctx->fs, from, to,
                    ((flags & 1) ? UV_FS_SYMLINK_DIR : 0) |
                    ((flags & 2) ? UV_FS_SYMLINK_JUNCTION : 0), aio_fs_cb)
    : uv_fs_link(al->loop, &ctx->fs, from, to, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_readlink(void *al_, const char *path, int realpath,
                         int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = realpath
    ? uv_fs_realpath(al->loop, &ctx->fs, path, aio_fs_cb)
    : uv_fs_readlink(al->loop, &ctx->fs, path, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

int64_t aio_fs_result_string_length(void *ctx_) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)ctx_;
  const char *s = (const char *)uv_fs_get_ptr(&ctx->fs);
  return s ? (int64_t)strlen(s) : -1;
}

int aio_fs_result_string_copy(void *ctx_, char *buf, int64_t buflen) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)ctx_;
  const char *s = (const char *)uv_fs_get_ptr(&ctx->fs);
  size_t n;
  if (!s) return UV_EINVAL;
  n = strlen(s);
  if (!buf || buflen <= 0 || n >= (size_t)buflen) return UV_ENOBUFS;
  memcpy(buf, s, n + 1);
  return 0;
}

int64_t aio_fs_result_path_length(void *ctx_) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)ctx_;
  const char *s = uv_fs_get_path(&ctx->fs);
  return s ? (int64_t)strlen(s) : -1;
}

int aio_fs_result_path_copy(void *ctx_, char *buf, int64_t buflen) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)ctx_;
  const char *s = uv_fs_get_path(&ctx->fs);
  size_t n;
  if (!s) return UV_EINVAL;
  n = strlen(s);
  if (!buf || buflen <= 0 || n >= (size_t)buflen) return UV_ENOBUFS;
  memcpy(buf, s, n + 1);
  return 0;
}

intptr_t aio_fs_chown(void *al_, const char *path, int64_t fd, int64_t uid,
                      int64_t gid, int kind, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  if (kind == 1)
    r = uv_fs_fchown(al->loop, &ctx->fs, (uv_file)fd,
                     (uv_uid_t)uid, (uv_gid_t)gid, aio_fs_cb);
  else if (kind == 2)
    r = uv_fs_lchown(al->loop, &ctx->fs, path,
                     (uv_uid_t)uid, (uv_gid_t)gid, aio_fs_cb);
  else
    r = uv_fs_chown(al->loop, &ctx->fs, path,
                    (uv_uid_t)uid, (uv_gid_t)gid, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

intptr_t aio_fs_statfs(void *al_, const char *path, int64_t reqid) {
  aio_loop_t *al = (aio_loop_t *)al_;
  aio_fs_ctx_t *ctx = aio_fs_ctx_new(reqid);
  int r;
  if (!ctx) return UV_ENOMEM;
  r = uv_fs_statfs(al->loop, &ctx->fs, path, aio_fs_cb);
  if (r != 0) return aio_fs_submit_failed(ctx, r);
  return (intptr_t)ctx;
}

uint64_t aio_fs_statfs_field(void *ctx_, int index) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)ctx_;
  const uv_statfs_t *st = (const uv_statfs_t *)uv_fs_get_ptr(&ctx->fs);
  if (!st) return 0;
  switch (index) {
    case 0: return st->f_type;
    case 1: return st->f_bsize;
    case 2: return st->f_blocks;
    case 3: return st->f_bfree;
    case 4: return st->f_bavail;
    case 5: return st->f_files;
    case 6: return st->f_ffree;
    case 7: return st->f_frsize;
    case 8: return st->f_spare[0];
    case 9: return st->f_spare[1];
    case 10: return st->f_spare[2];
    default: return 0;
  }
}

/* ---------------------------------------------------------- errors ------ */

void aio_strerror_into(int64_t code, char *buf, int64_t buflen) {
  const char *s = uv_strerror((int)code);
  if (!buf || buflen <= 0) return;
  strncpy(buf, s, (size_t)buflen - 1);
  buf[buflen - 1] = 0;
}

void aio_err_name_into(int64_t code, char *buf, int64_t buflen) {
  const char *s = uv_err_name((int)code);
  if (!buf || buflen <= 0) return;
  strncpy(buf, s, (size_t)buflen - 1);
  buf[buflen - 1] = 0;
}

int64_t aio_eof_code(void) { return UV_EOF; }

int64_t aio_eagain_code(void) { return UV_EAGAIN; }

/* Register the statically linked shim with Chez's foreign-entry table. */
void S_asyncio_init(void) {
#define AIO_REGISTER(name) Sforeign_symbol(#name, (void *)name)
  AIO_REGISTER(aio_loop_open);
  AIO_REGISTER(aio_completion_pop);
  AIO_REGISTER(aio_loop_run);
  AIO_REGISTER(aio_loop_alive);
  AIO_REGISTER(aio_loop_destroy);
  AIO_REGISTER(aio_wakeup_init);
  AIO_REGISTER(aio_wakeup_send);
  AIO_REGISTER(aio_bridge_init);
  AIO_REGISTER(aio_bridge_start);
  AIO_REGISTER(aio_bridge_stop);
  AIO_REGISTER(aio_handle_close);
  AIO_REGISTER(aio_handle_is_closing);
  AIO_REGISTER(aio_tcp_init);
  AIO_REGISTER(aio_tcp_bind);
  AIO_REGISTER(aio_listen_start);
  AIO_REGISTER(aio_accept);
  AIO_REGISTER(aio_tcp_connect);
  AIO_REGISTER(aio_pipe_init);
  AIO_REGISTER(aio_pipe_bind);
  AIO_REGISTER(aio_pipe_connect);
  AIO_REGISTER(aio_read_start);
  AIO_REGISTER(aio_read_stop);
  AIO_REGISTER(aio_read_copy);
  AIO_REGISTER(aio_free);
  Sforeign_symbol("chez_aio_write", (void *)chez_aio_write);
  AIO_REGISTER(aio_shutdown);
  AIO_REGISTER(aio_udp_init);
  AIO_REGISTER(aio_udp_bind);
  AIO_REGISTER(aio_udp_connect);
  AIO_REGISTER(aio_udp_recv_start);
  AIO_REGISTER(aio_udp_recv_stop);
  AIO_REGISTER(aio_udp_recv_copy);
  AIO_REGISTER(aio_udp_recv_addr);
  AIO_REGISTER(aio_udp_recv_free);
  AIO_REGISTER(aio_udp_send);
  AIO_REGISTER(aio_udp_address);
  AIO_REGISTER(aio_udp_set_membership);
  AIO_REGISTER(aio_udp_set_option);
  AIO_REGISTER(aio_udp_set_multicast_interface);
  AIO_REGISTER(aio_dns_lookup);
  AIO_REGISTER(aio_dns_cancel);
  AIO_REGISTER(aio_dns_count);
  AIO_REGISTER(aio_dns_addr);
  AIO_REGISTER(aio_dns_free);
  AIO_REGISTER(aio_dns_reverse);
  AIO_REGISTER(aio_dns_reverse_cancel);
  AIO_REGISTER(aio_dns_reverse_copy);
  AIO_REGISTER(aio_dns_reverse_free);
  AIO_REGISTER(aio_random);
  AIO_REGISTER(aio_random_cancel);
  AIO_REGISTER(aio_random_copy);
  AIO_REGISTER(aio_random_free);
  AIO_REGISTER(aio_poll_init);
  AIO_REGISTER(aio_poll_start);
  AIO_REGISTER(aio_poll_stop);
  AIO_REGISTER(aio_process_spawn);
  AIO_REGISTER(aio_process_pid);
  AIO_REGISTER(aio_process_kill);
  AIO_REGISTER(aio_kill);
  AIO_REGISTER(aio_process_term_signal);
  AIO_REGISTER(aio_process_result_free);
  AIO_REGISTER(aio_signal_init);
  AIO_REGISTER(aio_signal_start);
  AIO_REGISTER(aio_signal_stop);
  AIO_REGISTER(aio_fs_event_init);
  AIO_REGISTER(aio_fs_event_start);
  AIO_REGISTER(aio_fs_event_stop);
  AIO_REGISTER(aio_fs_event_result_copy);
  AIO_REGISTER(aio_fs_event_result_free);
  AIO_REGISTER(aio_fs_poll_init);
  AIO_REGISTER(aio_fs_poll_start);
  AIO_REGISTER(aio_fs_poll_stop);
  AIO_REGISTER(aio_fs_poll_result_field);
  AIO_REGISTER(aio_fs_poll_result_free);
  AIO_REGISTER(aio_tty_init);
  AIO_REGISTER(aio_tty_set_mode);
  AIO_REGISTER(aio_tty_winsize);
  AIO_REGISTER(aio_tty_get_vterm_state);
  AIO_REGISTER(aio_tty_set_vterm_state);
  AIO_REGISTER(aio_tty_reset_mode);
  AIO_REGISTER(aio_system_u64);
  AIO_REGISTER(aio_system_double);
  AIO_REGISTER(aio_system_string);
  AIO_REGISTER(aio_uname_string);
  AIO_REGISTER(aio_cpu_info);
  AIO_REGISTER(aio_cpu_info_count);
  AIO_REGISTER(aio_cpu_info_model);
  AIO_REGISTER(aio_cpu_info_field);
  AIO_REGISTER(aio_cpu_info_free);
  AIO_REGISTER(aio_interface_info);
  AIO_REGISTER(aio_interface_count);
  AIO_REGISTER(aio_interface_name);
  AIO_REGISTER(aio_interface_address);
  AIO_REGISTER(aio_interface_internal);
  AIO_REGISTER(aio_interface_physical);
  AIO_REGISTER(aio_interface_free);
  AIO_REGISTER(aio_rusage);
  AIO_REGISTER(aio_rusage_field);
  AIO_REGISTER(aio_rusage_free);
  AIO_REGISTER(aio_loop_metric);
  AIO_REGISTER(aio_fs_open);
  AIO_REGISTER(aio_fs_read);
  AIO_REGISTER(aio_fs_write);
  AIO_REGISTER(aio_fs_close_fd);
  AIO_REGISTER(aio_fs_close_now);
  AIO_REGISTER(aio_fs_stat);
  AIO_REGISTER(aio_fs_fstat);
  AIO_REGISTER(aio_fs_unlink);
  AIO_REGISTER(aio_fs_rename);
  AIO_REGISTER(aio_fs_mkdir);
  AIO_REGISTER(aio_fs_rmdir);
  AIO_REGISTER(aio_fs_copyfile);
  AIO_REGISTER(aio_fs_mkdtemp);
  AIO_REGISTER(aio_fs_mkstemp);
  AIO_REGISTER(aio_fs_scandir);
  AIO_REGISTER(aio_fs_scandir_next);
  AIO_REGISTER(aio_fs_opendir);
  AIO_REGISTER(aio_fs_readdir);
  AIO_REGISTER(aio_fs_closedir);
  AIO_REGISTER(aio_fs_closedir_now);
  AIO_REGISTER(aio_fs_result_ptr);
  AIO_REGISTER(aio_fs_readdir_entry);
  AIO_REGISTER(aio_fs_fsync);
  AIO_REGISTER(aio_fs_fdatasync);
  AIO_REGISTER(aio_fs_ftruncate);
  AIO_REGISTER(aio_fs_sendfile);
  AIO_REGISTER(aio_fs_access);
  AIO_REGISTER(aio_fs_chmod);
  AIO_REGISTER(aio_fs_fchmod);
  AIO_REGISTER(aio_fs_utime);
  AIO_REGISTER(aio_fs_futime);
  AIO_REGISTER(aio_fs_lstat);
  AIO_REGISTER(aio_fs_link);
  AIO_REGISTER(aio_fs_readlink);
  AIO_REGISTER(aio_fs_result_string_length);
  AIO_REGISTER(aio_fs_result_string_copy);
  AIO_REGISTER(aio_fs_result_path_length);
  AIO_REGISTER(aio_fs_result_path_copy);
  AIO_REGISTER(aio_fs_chown);
  AIO_REGISTER(aio_fs_statfs);
  AIO_REGISTER(aio_fs_statfs_field);
  AIO_REGISTER(aio_fs_req_free);
  AIO_REGISTER(aio_fs_cancel);
  AIO_REGISTER(aio_fs_data);
  AIO_REGISTER(aio_fs_buf_free);
  AIO_REGISTER(aio_fs_stat_field);
  AIO_REGISTER(aio_strerror_into);
  AIO_REGISTER(aio_err_name_into);
  AIO_REGISTER(aio_eof_code);
  AIO_REGISTER(aio_eagain_code);
#undef AIO_REGISTER
}
