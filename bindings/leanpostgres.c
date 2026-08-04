/*
 * Copyright (c) 2026 Paul Butcher. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
#include <lean/lean.h>
#include <libpq-fe.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>

static lean_external_class *g_pg_conn_class = NULL;
static lean_external_class *g_pg_result_class = NULL;

// The read/write ends of a `pipe(2)` used purely to wake a blocked `poll()` call in
// `leanpostgres_poll` when a new wait is registered on the Lean side (see `Postgres/Async.lean`);
// never exposed to Lean, so there's no need to encode them into a Lean-visible return type.
static int g_wake_read_fd = -1;
static int g_wake_write_fd = -1;

static void pg_conn_finalize(void *conn) {
    PQfinish((PGconn *)conn);
}

static void pg_conn_foreach(void *mod, b_lean_obj_arg fn) {
    (void)mod;
    (void)fn;
}

static void pg_result_finalize(void *result) {
    PQclear((PGresult *)result);
}

static void pg_result_foreach(void *mod, b_lean_obj_arg fn) {
    (void)mod;
    (void)fn;
}

LEAN_EXPORT lean_object *leanpostgres_initialize() {
    g_pg_conn_class = lean_register_external_class(pg_conn_finalize, pg_conn_foreach);
    g_pg_result_class = lean_register_external_class(pg_result_finalize, pg_result_foreach);

    // Best-effort: if `pipe()` fails (exceedingly rare, e.g. fd exhaustion), the wake fds stay -1;
    // `leanpostgres_poll` treats a negative fd as "ignore this entry" per POSIX `poll()` semantics,
    // so async waits still work, they just won't wake early when a new one is registered while
    // already blocked. Not worth failing module load over.
    int fds[2];
    if (pipe(fds) == 0) {
        fcntl(fds[0], F_SETFL, O_NONBLOCK);
        fcntl(fds[1], F_SETFL, O_NONBLOCK);
        g_wake_read_fd = fds[0];
        g_wake_write_fd = fds[1];
    }

    return lean_io_result_mk_ok(lean_box(0));
}

// Builds an `IO.Error.userError` whose message is `Postgres.Error.toString`'s format
// (`"[sqlstate] message"`), so `Postgres.Error.ofIOError?` can recover it on the Lean side.
// `sqlstate` is empty for connection-level failures, which precede any result to read one from.
static lean_object *leanpostgres_mk_error(const char *sqlstate, const char *message) {
    if (sqlstate == NULL) sqlstate = "";
    if (message == NULL) message = "";
    int needed = snprintf(NULL, 0, "[%s] %s", sqlstate, message);
    char *buf = malloc((size_t)needed + 1);
    snprintf(buf, (size_t)needed + 1, "[%s] %s", sqlstate, message);
    lean_object *msg_obj = lean_mk_string(buf);
    free(buf);
    return lean_io_result_mk_error(lean_mk_io_user_error(msg_obj));
}

LEAN_EXPORT lean_object *leanpostgres_open(lean_object *conninfo) {
    const char *conninfo_str = lean_string_cstr(conninfo);
    PGconn *conn = PQconnectdb(conninfo_str);
    lean_dec(conninfo);
    if (PQstatus(conn) != CONNECTION_OK) {
        lean_object *err = leanpostgres_mk_error("", PQerrorMessage(conn));
        PQfinish(conn);
        return err;
    }
    return lean_io_result_mk_ok(lean_alloc_external(g_pg_conn_class, conn));
}

// Each element of `params` is an `Option String`: `none` (a scalar) becomes a NULL parameter,
// `some s` becomes `s`'s text. `params` is borrowed; the caller still owns releasing it. The
// returned array must be `free`'d by the caller.
static const char **encode_params(b_lean_obj_arg params, size_t *out_n) {
    size_t nparams = lean_array_size(params);
    const char **values = NULL;
    if (nparams > 0) {
        values = malloc(sizeof(char *) * nparams);
        for (size_t i = 0; i < nparams; i++) {
            lean_object *opt = lean_array_get_core(params, i);
            values[i] = lean_is_scalar(opt) ? NULL : lean_string_cstr(lean_ctor_get(opt, 0));
        }
    }
    *out_n = nparams;
    return values;
}

// `conn` is borrowed; `sql` and `params` are consumed. Always runs through `PQexecParams` with
// null type/format arrays, i.e. text in, text out, per the design's text-only wire format.
LEAN_EXPORT lean_object *leanpostgres_exec_params(b_lean_obj_arg conn_obj, lean_object *sql, lean_object *params) {
    PGconn *conn = (PGconn *)lean_get_external_data(conn_obj);
    const char *sql_str = lean_string_cstr(sql);

    size_t nparams;
    const char **values = encode_params(params, &nparams);

    PGresult *result = PQexecParams(conn, sql_str, (int)nparams, NULL, values, NULL, NULL, 0);

    free(values);
    lean_dec(sql);
    lean_dec(params);

    ExecStatusType status = PQresultStatus(result);
    if (status != PGRES_TUPLES_OK && status != PGRES_COMMAND_OK) {
        char *sqlstate = PQresultErrorField(result, PG_DIAG_SQLSTATE);
        lean_object *err = leanpostgres_mk_error(sqlstate, PQresultErrorMessage(result));
        PQclear(result);
        return err;
    }

    return lean_io_result_mk_ok(lean_alloc_external(g_pg_result_class, result));
}

LEAN_EXPORT int32_t leanpostgres_ntuples(b_lean_obj_arg result) {
    return (int32_t)PQntuples((const PGresult *)lean_get_external_data(result));
}

LEAN_EXPORT int32_t leanpostgres_nfields(b_lean_obj_arg result) {
    return (int32_t)PQnfields((const PGresult *)lean_get_external_data(result));
}

LEAN_EXPORT lean_object *leanpostgres_getvalue(b_lean_obj_arg result, int32_t row, int32_t col) {
    const char *value = PQgetvalue((const PGresult *)lean_get_external_data(result), (int)row, (int)col);
    return lean_io_result_mk_ok(lean_mk_string(value));
}

LEAN_EXPORT uint8_t leanpostgres_getisnull(b_lean_obj_arg result, int32_t row, int32_t col) {
    return PQgetisnull((const PGresult *)lean_get_external_data(result), (int)row, (int)col) != 0;
}

LEAN_EXPORT lean_object *leanpostgres_fname(b_lean_obj_arg result, int32_t col) {
    const char *name = PQfname((PGresult *)lean_get_external_data(result), (int)col);
    return lean_io_result_mk_ok(lean_mk_string(name != NULL ? name : ""));
}

// The command tag of the most recently executed command (e.g. "SELECT", "INSERT 0 3"). Empty for
// a `PGresult` that was never assigned one (shouldn't occur for anything `exec_params` returns).
LEAN_EXPORT lean_object *leanpostgres_cmd_status(b_lean_obj_arg result) {
    const char *status = PQcmdStatus((PGresult *)lean_get_external_data(result));
    return lean_io_result_mk_ok(lean_mk_string(status != NULL ? status : ""));
}

// The number of rows affected by INSERT/UPDATE/DELETE/MOVE/FETCH/COPY, as decimal text; empty if
// the command doesn't produce one (e.g. SELECT, DDL).
LEAN_EXPORT lean_object *leanpostgres_cmd_tuples(b_lean_obj_arg result) {
    const char *tuples = PQcmdTuples((PGresult *)lean_get_external_data(result));
    return lean_io_result_mk_ok(lean_mk_string(tuples != NULL ? tuples : ""));
}

// The OID of the table a result column directly references, or 0 (InvalidOid) if it's a computed
// expression rather than a direct table-column reference.
LEAN_EXPORT uint32_t leanpostgres_ftable(b_lean_obj_arg result, int32_t col) {
    return (uint32_t)PQftable((PGresult *)lean_get_external_data(result), (int)col);
}

// The attribute (column) number within the table `leanpostgres_ftable` identifies, or 0 if none.
LEAN_EXPORT int32_t leanpostgres_ftablecol(b_lean_obj_arg result, int32_t col) {
    return (int32_t)PQftablecol((PGresult *)lean_get_external_data(result), (int)col);
}

LEAN_EXPORT lean_object *leanpostgres_db(b_lean_obj_arg conn) {
    const char *db = PQdb((PGconn *)lean_get_external_data(conn));
    return lean_io_result_mk_ok(lean_mk_string(db != NULL ? db : ""));
}

// --- Non-blocking query execution, driven by `Postgres.Async`'s poller loop. ---

// `enable = true` switches `conn` into libpq's non-blocking mode (required before
// `leanpostgres_send_query_params` and friends will avoid blocking); `enable = false` switches it
// back. Sync `step`/`exec` (via `leanpostgres_exec_params`/`PQexecParams`) is documented by libpq
// as unreliable on a connection left in non-blocking mode, so `Stmt.stepAsync` always restores
// blocking mode before returning, even on error.
LEAN_EXPORT lean_object *leanpostgres_set_nonblocking(b_lean_obj_arg conn_obj, uint8_t enable) {
    PGconn *conn = (PGconn *)lean_get_external_data(conn_obj);
    if (PQsetnonblocking(conn, enable ? 1 : 0) != 0) {
        return leanpostgres_mk_error("", PQerrorMessage(conn));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// The connection's underlying socket fd, to `poll()` on; `-1` if not currently connected.
LEAN_EXPORT int32_t leanpostgres_socket(b_lean_obj_arg conn_obj) {
    return (int32_t)PQsocket((PGconn *)lean_get_external_data(conn_obj));
}

// Queues `sql`/`params` for asynchronous execution (`PQsendQueryParams`); returns once the command
// has been queued, not once it's complete. `conn` must already be in non-blocking mode. `sql` and
// `params` are consumed, same convention as `leanpostgres_exec_params`.
LEAN_EXPORT lean_object *leanpostgres_send_query_params(b_lean_obj_arg conn_obj, lean_object *sql, lean_object *params) {
    PGconn *conn = (PGconn *)lean_get_external_data(conn_obj);
    const char *sql_str = lean_string_cstr(sql);

    size_t nparams;
    const char **values = encode_params(params, &nparams);

    int ok = PQsendQueryParams(conn, sql_str, (int)nparams, NULL, values, NULL, NULL, 0);
    free(values);

    lean_object *result = ok
        ? lean_io_result_mk_ok(lean_box(0))
        : leanpostgres_mk_error("", PQerrorMessage(conn));

    lean_dec(sql);
    lean_dec(params);
    return result;
}

// Attempts to send any data still buffered from `leanpostgres_send_query_params`. Returns `true`
// (as a boxed `Bool`) if more remains to be flushed once the socket is next writable, `false` if
// fully flushed.
LEAN_EXPORT lean_object *leanpostgres_flush(b_lean_obj_arg conn_obj) {
    PGconn *conn = (PGconn *)lean_get_external_data(conn_obj);
    int rc = PQflush(conn);
    if (rc < 0) {
        return leanpostgres_mk_error("", PQerrorMessage(conn));
    }
    return lean_io_result_mk_ok(lean_box(rc != 0 ? 1 : 0));
}

// Reads whatever is currently available from the socket into libpq's internal buffers
// (`PQconsumeInput`); does not itself block. Call after the socket is reported readable, then
// re-check `leanpostgres_is_busy`.
LEAN_EXPORT lean_object *leanpostgres_consume_input(b_lean_obj_arg conn_obj) {
    PGconn *conn = (PGconn *)lean_get_external_data(conn_obj);
    if (PQconsumeInput(conn) == 0) {
        return leanpostgres_mk_error("", PQerrorMessage(conn));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// Whether a call to `leanpostgres_get_result` would currently block. Just inspects buffered state
// (no socket I/O), so unlike `leanpostgres_flush`/`leanpostgres_consume_input` this can't fail.
LEAN_EXPORT uint8_t leanpostgres_is_busy(b_lean_obj_arg conn_obj) {
    return PQisBusy((PGconn *)lean_get_external_data(conn_obj)) != 0;
}

// Fetches the result of the command sent via `leanpostgres_send_query_params`, once
// `leanpostgres_is_busy` reports `false`. Mirrors `leanpostgres_exec_params`'s status-checking
// tail exactly, for the same error-format guarantee. Drains any further `PQgetResult` calls
// (expected to yield NULL, marking end-of-command); `Stmt.sql` is always a single statement, same
// restriction `PQexecParams` already has, so this is defensive rather than a real multi-result path.
LEAN_EXPORT lean_object *leanpostgres_get_result(b_lean_obj_arg conn_obj) {
    PGconn *conn = (PGconn *)lean_get_external_data(conn_obj);
    PGresult *result = PQgetResult(conn);
    if (result == NULL) {
        return leanpostgres_mk_error("", "no result available from PQgetResult");
    }

    ExecStatusType status = PQresultStatus(result);
    if (status != PGRES_TUPLES_OK && status != PGRES_COMMAND_OK) {
        char *sqlstate = PQresultErrorField(result, PG_DIAG_SQLSTATE);
        lean_object *err = leanpostgres_mk_error(sqlstate, PQresultErrorMessage(result));
        PQclear(result);
        PGresult *extra;
        while ((extra = PQgetResult(conn)) != NULL) PQclear(extra);
        return err;
    }

    PGresult *extra;
    while ((extra = PQgetResult(conn)) != NULL) PQclear(extra);
    return lean_io_result_mk_ok(lean_alloc_external(g_pg_result_class, result));
}

// --- Generic socket-readiness polling, backing `Postgres.Async`'s single poller loop. ---

// Blocks until at least one of `fds[i]` becomes ready for reading (`want_write[i] = false`) or
// writing (`want_write[i] = true`), or a new wait is registered elsewhere via `leanpostgres_wake`
// (see the wake pipe set up in `leanpostgres_initialize`). Returns a same-length `Bool` array of
// which entries are ready; a fatal `poll()` failure (not the ordinary case of an fd erroring, which
// is reported as "ready" so the caller's own libpq call surfaces the real error) throws instead.
LEAN_EXPORT lean_object *leanpostgres_poll(lean_object *fds, lean_object *want_write) {
    size_t n = lean_array_size(fds);
    struct pollfd *pfds = malloc(sizeof(struct pollfd) * (n + 1));
    for (size_t i = 0; i < n; i++) {
        pfds[i].fd = (int32_t)lean_unbox_uint32(lean_array_get_core(fds, i));
        uint8_t want_write_i = (uint8_t)lean_unbox(lean_array_get_core(want_write, i));
        pfds[i].events = want_write_i ? POLLOUT : POLLIN;
        pfds[i].revents = 0;
    }
    pfds[n].fd = g_wake_read_fd;
    pfds[n].events = POLLIN;
    pfds[n].revents = 0;

    int rc;
    do {
        rc = poll(pfds, n + 1, -1);
    } while (rc < 0 && errno == EINTR);

    if (rc < 0) {
        lean_object *err = leanpostgres_mk_error("", strerror(errno));
        free(pfds);
        lean_dec(fds);
        lean_dec(want_write);
        return err;
    }

    if (pfds[n].revents != 0) {
        char buf[256];
        while (read(g_wake_read_fd, buf, sizeof(buf)) > 0) {}
    }

    lean_object *out = lean_alloc_array(n, n);
    for (size_t i = 0; i < n; i++) {
        lean_array_set_core(out, i, lean_box(pfds[i].revents != 0 ? 1 : 0));
    }

    free(pfds);
    lean_dec(fds);
    lean_dec(want_write);
    return lean_io_result_mk_ok(out);
}

// Wakes a `leanpostgres_poll` call currently blocked in another thread, so it notices a freshly
// registered wait without having to wait for an unrelated fd to become ready first. A no-op if the
// wake pipe failed to set up at initialization (see `leanpostgres_initialize`).
LEAN_EXPORT lean_object *leanpostgres_wake() {
    if (g_wake_write_fd >= 0) {
        char b = 0;
        while (write(g_wake_write_fd, &b, 1) < 0 && errno == EINTR) {}
    }
    return lean_io_result_mk_ok(lean_box(0));
}
