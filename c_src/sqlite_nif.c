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

static ErlNifFunc esqlite_nif_functions[] = {
        {"libversion", 0, esqlite_libversion, 0},
        {"sourceid", 0, esqlite_sourceid, 0},

        {"open", 3, esqlite_open, 0},
        {"close", 1, esqlite_close, 0},

        {"prepare", 3, esqlite_prepare, 0},
        {"finalize", 1, esqlite_finalize, 0},
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
