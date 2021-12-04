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

static int esqlite_inspect_open_flags(ErlNifEnv *, ERL_NIF_TERM, int *);

void
esqlite_database_delete(ErlNifEnv *env, void *ptr) {
        // While sqlite3_close_v2() has a return value, it always returns
        // SQLITE3_OK (which is why we use it instead of sqlite3_close()).
        sqlite3_close_v2((struct sqlite3 *)ptr);
}

int
esqlite_inspect_database(ErlNifEnv *env, ERL_NIF_TERM term,
                         struct sqlite3 **opdb) {
        struct esqlite_nif_data *nif_data;
        ErlNifResourceType *resource_type;
        struct sqlite3 **pdb;

        nif_data = enif_priv_data(env);
        resource_type = nif_data->database_resource_type;

        if (enif_get_resource(env, term, resource_type, (void **)&pdb) == 0)
                return 0;

        *opdb = *pdb;

        return 1;
}

ERL_NIF_TERM
esqlite_database_error_code(ErlNifEnv *env, struct sqlite3 *db) {
        int code;

        code = sqlite3_extended_errcode(db);

        return esqlite_result_code(env, code);
}

ERL_NIF_TERM
esqlite_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct esqlite_nif_data *nif_data;
        struct sqlite3 *db, **pdb;
        char *path, *vfs;
        int flags, ret;

        nif_data = enif_priv_data(env);

        if (argc != 3)
                return enif_make_badarg(env);

        if (esqlite_inspect_binary_string(env, argv[0], &path) == 0)
                return enif_make_badarg(env);

        if (esqlite_inspect_open_flags(env, argv[1], &flags) == 0)
                return enif_make_badarg(env);

        if (esqlite_is_atom(env, argv[2], "undefined")) {
                vfs = NULL;
        } else if (esqlite_inspect_binary_string(env, argv[2], &vfs) == 0) {
                return enif_make_badarg(env);
        }

        ret = sqlite3_open_v2(path, &db, flags, vfs);
        if (!db || ret != SQLITE_OK) {
                ERL_NIF_TERM reason;

                reason = esqlite_database_error_code(env, db);

                enif_free(path);
                enif_free(vfs);

                return esqlite_error_tuple(env, reason);
        }

        enif_free(path);
        enif_free(vfs);

        sqlite3_extended_result_codes(db, 1);

        pdb = enif_alloc_resource(nif_data->database_resource_type,
                                  sizeof(*pdb));
        *pdb = db;

        return esqlite_ok_tuple(env, enif_make_resource(env, pdb));
}

ERL_NIF_TERM
esqlite_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct sqlite3 *db;

        if (argc != 1)
                return enif_make_badarg(env);

        if (esqlite_inspect_database(env, argv[0], &db) == 0)
                return enif_make_badarg(env);

        sqlite3_close_v2(db);

        return enif_make_atom(env, "ok");
}

static int
esqlite_inspect_open_flags(ErlNifEnv *env, ERL_NIF_TERM list, int *pflags) {
        ERL_NIF_TERM head, tail;
        int flags;

        if (!enif_is_list(env, list))
                return 0;

        flags = 0;

        while (enif_get_list_cell(env, list, &head, &tail) == 1) {
                if (esqlite_is_atom(env, head, "readonly")) {
                        flags |= SQLITE_OPEN_READONLY;
                } else if (esqlite_is_atom(env, head, "readwrite")) {
                        flags |= SQLITE_OPEN_READWRITE;
                } else if (esqlite_is_atom(env, head, "create")) {
                        flags |= SQLITE_OPEN_CREATE;
                } else if (esqlite_is_atom(env, head, "uri")) {
                        flags |= SQLITE_OPEN_URI;
                } else if (esqlite_is_atom(env, head, "memory")) {
                        flags |= SQLITE_OPEN_MEMORY;
                } else if (esqlite_is_atom(env, head, "nomutex")) {
                        flags |= SQLITE_OPEN_NOMUTEX;
                } else if (esqlite_is_atom(env, head, "fullmutex")) {
                        flags |= SQLITE_OPEN_FULLMUTEX;
                } else if (esqlite_is_atom(env, head, "sharedcache")) {
                        flags |= SQLITE_OPEN_SHAREDCACHE;
                } else if (esqlite_is_atom(env, head, "privatecache")) {
                        flags |= SQLITE_OPEN_PRIVATECACHE;
                } else if (esqlite_is_atom(env, head, "nofollow")) {
                        flags |= SQLITE_OPEN_NOFOLLOW;
                } else {
                        return 0;
                }

                list = tail;
        }

        *pflags = flags;

        return 1;
}
