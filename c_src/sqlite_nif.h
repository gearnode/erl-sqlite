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

#ifndef SQLITE_NIF_H
#define SQLITE_NIF_H

#include <stdbool.h>
#include <string.h>

#include <ei.h>
#include <erl_nif.h>

#include "sqlite3.h"

struct esqlite_nif_data {
        ErlNifResourceType *database_resource_type;
};

void esqlite_database_delete(ErlNifEnv *, void *);

ERL_NIF_TERM esqlite_ok_tuple(ErlNifEnv *, ERL_NIF_TERM);
ERL_NIF_TERM esqlite_error_tuple(ErlNifEnv *, ERL_NIF_TERM);
ERL_NIF_TERM esqlite_binary_string(ErlNifEnv *, const char *);
int esqlite_inspect_binary_string(ErlNifEnv *, ERL_NIF_TERM, char **);
bool esqlite_is_atom(ErlNifEnv *env, ERL_NIF_TERM, const char *);
ErlNifResourceType *esqlite_create_resource_type(ErlNifEnv *, const char *,
                                                 ErlNifResourceDtor *);

#define ESQLITE_EXPORT(name_) \
        ERL_NIF_TERM name_(ErlNifEnv *, int, const ERL_NIF_TERM [])

ESQLITE_EXPORT(esqlite_libversion);
ESQLITE_EXPORT(esqlite_sourceid);

ESQLITE_EXPORT(esqlite_open);
ESQLITE_EXPORT(esqlite_close);

#endif
