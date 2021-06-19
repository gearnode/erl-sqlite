// Copyright (c) 2021 Nicolas Martyanoff <khaelin@gmail.com>.
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
// SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
// IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

#include "sqlite_nif.h"

static int
esqlite_inspect_prepare_flags(ErlNifEnv *, ERL_NIF_TERM, unsigned int *);

void
esqlite_statement_delete(ErlNifEnv *env, void *ptr) {
        sqlite3_finalize((struct sqlite3_stmt *)ptr);
}

int
esqlite_inspect_statement(ErlNifEnv *env, ERL_NIF_TERM term,
                         struct sqlite3_stmt **opstmt) {
        struct esqlite_nif_data *nif_data;
        ErlNifResourceType *resource_type;
        struct sqlite3_stmt **pstmt;
        int ret;

        nif_data = enif_priv_data(env);
        resource_type = nif_data->statement_resource_type;

        ret = enif_get_resource(env, term, resource_type, (void **)&pstmt);
        if (ret == 0)
                return 0;

        *opstmt = *pstmt;

        return 1;
}

ERL_NIF_TERM
esqlite_prepare(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct esqlite_nif_data *nif_data;
        struct sqlite3 *db;
        struct sqlite3_stmt *stmt, **pstmt;
        ERL_NIF_TERM stmt_resource, tail_term;
        const char *tail;
        char *query;
        unsigned int flags;
        int ret;

        nif_data = enif_priv_data(env);

        if (argc != 3)
                return enif_make_badarg(env);

        if (esqlite_inspect_database(env, argv[0], &db) == 0)
                return enif_make_badarg(env);

        if (esqlite_inspect_binary_string(env, argv[1], &query) == 0)
                return enif_make_badarg(env);

        if (esqlite_inspect_prepare_flags(env, argv[2], &flags) == 0)
                return enif_make_badarg(env);

        ret = sqlite3_prepare_v3(db, query, -1, flags, &stmt, &tail);
        if (ret != SQLITE_OK) {
                ERL_NIF_TERM reason;

                reason = esqlite_result_code(env, ret);

                enif_free(query);

                return esqlite_error_tuple(env, reason);
        }

        tail_term = esqlite_binary_string(env, tail);

        enif_free(query);

        pstmt = enif_alloc_resource(nif_data->statement_resource_type,
                                    sizeof(*pstmt));
        *pstmt = stmt;
        stmt_resource = enif_make_resource(env, pstmt);

        return esqlite_ok_tuple2(env, stmt_resource, tail_term);
}

ERL_NIF_TERM
esqlite_finalize(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct esqlite_nif_data *nif_data;
        struct sqlite3_stmt *stmt;

        nif_data = enif_priv_data(env);

        if (argc != 1)
                return enif_make_badarg(env);

        if (esqlite_inspect_statement(env, argv[0], &stmt) == 0)
                return enif_make_badarg(env);

        sqlite3_finalize(stmt);

        return enif_make_atom(env, "ok");
}

ERL_NIF_TERM
esqlite_step(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct esqlite_nif_data *nif_data;
        struct sqlite3_stmt *stmt;
        ERL_NIF_TERM reason;
        int ret;

        nif_data = enif_priv_data(env);

        if (argc != 1)
                return enif_make_badarg(env);

        if (esqlite_inspect_statement(env, argv[0], &stmt) == 0)
                return enif_make_badarg(env);

        ret = sqlite3_step(stmt);

        reason = esqlite_result_code(env, ret);

        if (ret != SQLITE_ROW && ret != SQLITE_DONE)
                return esqlite_error_tuple(env, reason);

        return esqlite_ok_tuple(env, reason);
}

ERL_NIF_TERM
esqlite_reset(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct esqlite_nif_data *nif_data;
        struct sqlite3_stmt *stmt;
        int ret;

        nif_data = enif_priv_data(env);

        if (argc != 1)
                return enif_make_badarg(env);

        if (esqlite_inspect_statement(env, argv[0], &stmt) == 0)
                return enif_make_badarg(env);

        ret = sqlite3_reset(stmt);
        if (ret != SQLITE_OK) {
                ERL_NIF_TERM reason;

                reason = esqlite_result_code(env, ret);

                return esqlite_error_tuple(env, reason);
        }

        return enif_make_atom(env, "ok");
}

ERL_NIF_TERM
esqlite_column_count(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct esqlite_nif_data *nif_data;
        struct sqlite3_stmt *stmt;
        int count;

        nif_data = enif_priv_data(env);

        if (argc != 1)
                return enif_make_badarg(env);

        if (esqlite_inspect_statement(env, argv[0], &stmt) == 0)
                return enif_make_badarg(env);

        count = sqlite3_column_count(stmt);

        return enif_make_int(env, count);
}

ERL_NIF_TERM
esqlite_column_type(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct esqlite_nif_data *nif_data;
        struct sqlite3_stmt *stmt;
        ERL_NIF_TERM error;
        int column, type;

        nif_data = enif_priv_data(env);

        if (argc != 2)
                return enif_make_badarg(env);

        if (esqlite_inspect_statement(env, argv[0], &stmt) == 0)
                return enif_make_badarg(env);

        if (enif_get_int(env, argv[1], &column) == 0)
                return enif_make_badarg(env);

        type = sqlite3_column_type(stmt, column);

        switch (type) {
        case SQLITE_BLOB:
                return enif_make_atom(env, "blob");
        case SQLITE_FLOAT:
                return enif_make_atom(env, "float");
        case SQLITE_INTEGER:
                return enif_make_atom(env, "integer");
        case SQLITE_NULL:
                return enif_make_atom(env, "null");
        case SQLITE_TEXT:
                return enif_make_atom(env, "text");
        }

        error = enif_make_tuple2(env, enif_make_atom(env, "unknown_datatype"),
                                 enif_make_int(env, type));
        return enif_raise_exception(env, error);
}

static int
esqlite_inspect_prepare_flags(ErlNifEnv *env, ERL_NIF_TERM list,
                              unsigned int *pflags) {
        ERL_NIF_TERM head, tail;
        unsigned int flags;

        if (!enif_is_list(env, list))
                return 0;

        flags = 0;

        while (enif_get_list_cell(env, list, &head, &tail) == 1) {
                if (esqlite_is_atom(env, head, "persistent")) {
                        flags |= SQLITE_PREPARE_PERSISTENT;
                } else if (esqlite_is_atom(env, head, "normalize")) {
                        flags |= SQLITE_PREPARE_NORMALIZE;
                } else if (esqlite_is_atom(env, head, "no_vtab")) {
                        flags |= SQLITE_PREPARE_NO_VTAB;
                } else {
                        return 0;
                }

                list = tail;
        }

        *pflags = flags;

        return 1;
}
