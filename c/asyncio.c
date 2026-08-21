/* asyncio.c - libuv shim for Chez Scheme async I/O.

   This shim owns the concrete layouts of uv_loop_t, uv_handle_t, and
   uv_req_t.  Scheme code refers to native objects through opaque pointers
   and stable integer identifiers registered in a Scheme-side registry.

   All user-visible events are reported through a single notify trampoline
   installed per loop; the trampoline runs only inside uv_run on the thread
   that owns the loop.  libuv worker threads never call back into Scheme. */

#if !defined(_WIN32) && !defined(_POSIX_C_SOURCE)
# define _POSIX_C_SOURCE 200112L
#endif

#include <uv.h>
#include "scheme.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <fcntl.h>
#include <limits.h>

/* event kinds delivered to the notify trampoline */
#define AIO_EV_ACCEPT    1  /* listener became readable: id = listener id */
#define AIO_EV_READ      2  /* stream read: status nread (>0), UV_EOF, or error */
#define AIO_EV_WRITE     3  /* status 0 or error */
#define AIO_EV_CONNECT   4  /* outbound connect: status 0 or error */
#define AIO_EV_SHUTDOWN  5
#define AIO_EV_FS        6  /* status = uv_fs_get_result */
#define AIO_EV_DNS       7  /* status 0 or error; aux = request context */
#define AIO_EV_CLOSE     8  /* handle close completed: id = handle id */

/* notify(loop, id, kind, status, aux) */
typedef void (*aio_notify_t)(void *loop, int64_t id, int64_t kind,
                             int64_t status, void *aux);

typedef struct {
  uv_loop_t *loop;
  aio_notify_t notify;
  uv_async_t *wakeup;    /* lets foreign threads interrupt a blocking uv_run */
  uv_timer_t *bridge;    /* fires at the nearest Scheme-side timer deadline */
} aio_loop_t;

/* per-request context for operations that carry C-owned data */
typedef struct {
  uv_fs_t fs;            /* also used as generic storage for fs requests */
  char *buf;             /* read destination or write copy, freed via aio_fs_buf_free */
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

static aio_loop_t *aio_loop_of(uv_loop_t *loop) {
  return (aio_loop_t *)uv_loop_get_data(loop);
}

static void aio_report(uv_loop_t *loop, int64_t id, int64_t kind,
                       int64_t status, void *aux) {
  aio_loop_t *al = aio_loop_of(loop);
  if (al->notify) al->notify(al, id, kind, status, aux);
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
  al->notify = NULL;
  al->wakeup = NULL;
  al->bridge = NULL;
  uv_loop_set_data(al->loop, al);
  return al;
}

void aio_set_notify(void *al_, void (*notify)(void *, int64_t, int64_t,
                                              int64_t, void *)) {
  aio_loop_t *al = (aio_loop_t *)al_;
  al->notify = notify;
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
  int r = uv_loop_close(al->loop);
  if (r == 0) {
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
  buf->base = (char *)malloc(65536);
  buf->len = buf->base ? 65536 : 0;
  (void)h;
}

static void aio_read_cb(uv_stream_t *stream, ssize_t nread,
                        const uv_buf_t *buf) {
  int64_t id = (int64_t)(intptr_t)uv_handle_get_data((uv_handle_t *)stream);
  if (nread == 0) {
    free(buf->base);
    return;
  }
  uv_read_stop(stream);
  if (nread > 0) {
    aio_report(stream->loop, id, AIO_EV_READ, nread, buf->base);
  } else {
    free(buf->base);
    aio_report(stream->loop, id, AIO_EV_READ, nread, NULL);
  }
}

/* copy n bytes from a completed read buffer; caller then frees it */
void aio_read_copy(void *buf, void *dest, int64_t n) {
  memcpy(dest, buf, (size_t)n);
}

void aio_free(void *p) { free(p); }

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
int aio_write(void *h, const void *src, int64_t len, int64_t reqid) {
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

/* -------------------------------------------------------------- fs ------ */

static void aio_fs_cb(uv_fs_t *req) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)req;
  int64_t id = (int64_t)(intptr_t)uv_req_get_data((uv_req_t *)req);
  aio_report(req->loop, id, AIO_EV_FS, uv_fs_get_result(req), ctx);
}

static aio_fs_ctx_t *aio_fs_ctx_new(int64_t reqid) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)malloc(sizeof(aio_fs_ctx_t));
  if (ctx) {
    memset(ctx, 0, sizeof(*ctx));
    ctx->buf = NULL;
    uv_req_set_data((uv_req_t *)&ctx->fs, (void *)(intptr_t)reqid);
  }
  return ctx;
}

static intptr_t aio_fs_submit_failed(aio_fs_ctx_t *ctx, int r) {
  uv_fs_req_cleanup(&ctx->fs);
  free(ctx->buf);
  free(ctx);
  return (intptr_t)r;
}

void aio_fs_req_free(void *ctx_) {
  aio_fs_ctx_t *ctx = (aio_fs_ctx_t *)ctx_;
  uv_fs_req_cleanup(&ctx->fs);
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
  AIO_REGISTER(aio_set_notify);
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
  AIO_REGISTER(aio_write);
  AIO_REGISTER(aio_shutdown);
  AIO_REGISTER(aio_dns_lookup);
  AIO_REGISTER(aio_dns_cancel);
  AIO_REGISTER(aio_dns_count);
  AIO_REGISTER(aio_dns_addr);
  AIO_REGISTER(aio_dns_free);
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
