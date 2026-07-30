#include <lean/lean.h>
#include <libpq-fe.h>
#include <stdio.h>
#include <stdlib.h>

static lean_external_class *g_pg_conn_class = NULL;
static lean_external_class *g_pg_result_class = NULL;

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
