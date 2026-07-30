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

// `conn` is borrowed; `sql` and `params` are consumed. Each element of `params` is an
// `Option String`: `none` (a scalar) becomes a NULL parameter, `some s` becomes `s`'s text.
// Always runs through `PQexecParams` with null type/format arrays, i.e. text in, text out,
// per the design's text-only wire format.
LEAN_EXPORT lean_object *leanpostgres_exec_params(b_lean_obj_arg conn_obj, lean_object *sql, lean_object *params) {
    PGconn *conn = (PGconn *)lean_get_external_data(conn_obj);
    const char *sql_str = lean_string_cstr(sql);

    size_t nparams = lean_array_size(params);
    const char **values = NULL;
    if (nparams > 0) {
        values = malloc(sizeof(char *) * nparams);
        for (size_t i = 0; i < nparams; i++) {
            lean_object *opt = lean_array_get_core(params, i);
            values[i] = lean_is_scalar(opt) ? NULL : lean_string_cstr(lean_ctor_get(opt, 0));
        }
    }

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
