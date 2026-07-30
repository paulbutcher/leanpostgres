#include <lean/lean.h>
#include <libpq-fe.h>

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

LEAN_EXPORT lean_object *leanpostgres_initialize(lean_object *w) {
    (void)w;
    g_pg_conn_class = lean_register_external_class(pg_conn_finalize, pg_conn_foreach);
    g_pg_result_class = lean_register_external_class(pg_result_finalize, pg_result_foreach);
    return lean_io_result_mk_ok(lean_box(0));
}
