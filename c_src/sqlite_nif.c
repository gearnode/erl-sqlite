// Copyright (c) 2021 Exograd SAS.
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

static ErlNifFunc esqlite_nif_functions[] = {
        // Misc
        {"libversion", 0, esqlite_libversion, 0},
        {"sourceid", 0, esqlite_sourceid, 0},

        // Databases
        {"open", 3, esqlite_open, 0},
        {"close", 1, esqlite_close, 0},

        // Statements
        {"prepare", 3, esqlite_prepare, ERL_NIF_DIRTY_JOB_CPU_BOUND},
        {"finalize", 1, esqlite_finalize, 0},
        {"step", 1, esqlite_step, ERL_NIF_DIRTY_JOB_CPU_BOUND},
        {"reset", 1, esqlite_reset, 0},

        {"column_count", 1, esqlite_column_count, 0},
        {"column_type", 2, esqlite_column_type, 0},
        {"column_bytes", 2, esqlite_column_bytes, 0},
        {"column_blob", 2, esqlite_column_blob, 0},
        {"column_double", 2, esqlite_column_double, 0},
        {"column_int64", 2, esqlite_column_int64, 0},
        {"column_text", 2, esqlite_column_text, 0},

        {"bind_blob64", 3, esqlite_bind_blob64, 0},
        {"bind_double", 3, esqlite_bind_double, 0},
        {"bind_int64", 3, esqlite_bind_int64, 0},
        {"bind_null", 2, esqlite_bind_null, 0},
        {"bind_text64", 3, esqlite_bind_text64, 0},
};

static int
esqlite_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info) {
        struct esqlite_nif_data *data;

        data = enif_alloc(sizeof(*data));
        if(data == NULL) {
                enif_fprintf(stderr, "cannot allocate nif data: %s\n",
                             strerror(errno));
                return 1;
        }

        data->database_resource_type =
                esqlite_create_resource_type(env, "database",
                                             esqlite_database_delete);
        data->statement_resource_type =
                esqlite_create_resource_type(env, "statement",
                                             esqlite_statement_delete);

        *priv = (void *)data;
        return 0;
}

static void
esqlite_unload(ErlNifEnv *env, void *priv) {
        struct esqlite_nif_data *data;

        data = priv;
        enif_free(data);
}


ERL_NIF_INIT(sqlite_nif, esqlite_nif_functions,
             esqlite_load, NULL, NULL, esqlite_unload);
