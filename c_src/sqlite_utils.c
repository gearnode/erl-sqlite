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

ERL_NIF_TERM
esqlite_ok_tuple(ErlNifEnv *env, ERL_NIF_TERM term) {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), term);
}

ERL_NIF_TERM
esqlite_ok_tuple2(ErlNifEnv *env, ERL_NIF_TERM term1, ERL_NIF_TERM term2) {
        return enif_make_tuple3(env, enif_make_atom(env, "ok"), term1, term2);
}

ERL_NIF_TERM
esqlite_error_tuple(ErlNifEnv *env, ERL_NIF_TERM term) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), term);
}

ERL_NIF_TERM
esqlite_binary_string(ErlNifEnv *env, const char *s) {
        ERL_NIF_TERM term;
        unsigned char *buf;
        size_t len;

        len = strlen(s);
        buf = enif_make_new_binary(env, len, &term);
        memcpy(buf, s, len);

        return term;
}

int
esqlite_inspect_binary_string(ErlNifEnv *env, ERL_NIF_TERM binary_term,
                              char **string) {
        ErlNifBinary binary;
        char *data;

        if (enif_inspect_binary(env, binary_term, &binary) == 0)
                return 0;

        data = enif_alloc(binary.size + 1);
        memcpy(data, binary.data, binary.size);
        data[binary.size] = '\0';

        *string = data;

        return 1;
}

bool
esqlite_is_atom(ErlNifEnv *env, ERL_NIF_TERM term, const char *name) {
        char atom_name[256]; // totally arbitrary max length
        int ret;

        ret = enif_get_atom(env, term, atom_name, sizeof(atom_name),
                            ERL_NIF_LATIN1);
        if (ret == -1)
                return false;

        return strcmp(atom_name, name) == 0;
}

ErlNifResourceType *
esqlite_create_resource_type(ErlNifEnv *env, const char *name,
                             ErlNifResourceDtor *dtor) {
        ErlNifResourceFlags flags;
        ErlNifResourceType *type;

        flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

        type = enif_open_resource_type(env, NULL, name, NULL, flags, NULL);
        if (type == NULL) {
                enif_fprintf(stderr, "cannot open resource type '%s'\n");
                return NULL;
        }

        return type;
}
