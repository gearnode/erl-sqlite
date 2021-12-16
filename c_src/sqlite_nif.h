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

#ifndef SQLITE_NIF_H
#define SQLITE_NIF_H

#include <stdbool.h>
#include <string.h>

#include <ei.h>
#include <erl_nif.h>

#include "sqlite3.h"

#define ESQLITE_EXPORT(name_) \
        ERL_NIF_TERM name_(ErlNifEnv *, int, const ERL_NIF_TERM [])

struct esqlite_nif_data {
        ErlNifResourceType *database_resource_type;
        ErlNifResourceType *statement_resource_type;
};

// Utils
ERL_NIF_TERM esqlite_ok_tuple(ErlNifEnv *, ERL_NIF_TERM);
ERL_NIF_TERM esqlite_ok_tuple2(ErlNifEnv *, ERL_NIF_TERM, ERL_NIF_TERM);
ERL_NIF_TERM esqlite_error_tuple(ErlNifEnv *, ERL_NIF_TERM);
ERL_NIF_TERM esqlite_binary_string(ErlNifEnv *, const char *);
int esqlite_inspect_binary_string(ErlNifEnv *, ERL_NIF_TERM, char **);
bool esqlite_is_atom(ErlNifEnv *env, ERL_NIF_TERM, const char *);
ErlNifResourceType *esqlite_create_resource_type(ErlNifEnv *, const char *,
                                                 ErlNifResourceDtor *);

// Result codes
ERL_NIF_TERM esqlite_result_code(ErlNifEnv *, int);

// Misc
ESQLITE_EXPORT(esqlite_libversion);
ESQLITE_EXPORT(esqlite_sourceid);

// Databases
void esqlite_database_delete(ErlNifEnv *, void *);
int esqlite_inspect_database(ErlNifEnv *, ERL_NIF_TERM, struct sqlite3 **);
ERL_NIF_TERM esqlite_database_error_code(ErlNifEnv *, struct sqlite3 *);

ESQLITE_EXPORT(esqlite_open);
ESQLITE_EXPORT(esqlite_close);

// Statements
void esqlite_statement_delete(ErlNifEnv *, void *);

ESQLITE_EXPORT(esqlite_prepare);
ESQLITE_EXPORT(esqlite_finalize);
ESQLITE_EXPORT(esqlite_step);
ESQLITE_EXPORT(esqlite_reset);

ESQLITE_EXPORT(esqlite_column_count);
ESQLITE_EXPORT(esqlite_column_type);
ESQLITE_EXPORT(esqlite_column_bytes);
ESQLITE_EXPORT(esqlite_column_blob);
ESQLITE_EXPORT(esqlite_column_double);
ESQLITE_EXPORT(esqlite_column_int64);
ESQLITE_EXPORT(esqlite_column_text);

ESQLITE_EXPORT(esqlite_bind_blob64);
ESQLITE_EXPORT(esqlite_bind_double);
ESQLITE_EXPORT(esqlite_bind_int64);
ESQLITE_EXPORT(esqlite_bind_null);
ESQLITE_EXPORT(esqlite_bind_text64);

#endif
