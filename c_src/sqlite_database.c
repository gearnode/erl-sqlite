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

static ERL_NIF_TERM esqlite_error_code(ErlNifEnv *, struct sqlite3 *);
static int esqlite_inspect_open_flags(ErlNifEnv *, ERL_NIF_TERM, int *);

void
esqlite_database_delete(ErlNifEnv *env, void *ptr) {
        struct sqlite3 *db;

        db = ptr;

        // While sqlite3_close_v2() has a return value, it always returns
        // SQLITE3_OK (which is why we use it instead of sqlite3_close()).
        sqlite3_close_v2(db);
}

ERL_NIF_TERM
esqlite_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct esqlite_nif_data *nif_data;
        struct sqlite3 *db, **db_resource;
        char *path, *vfs;
        int flags, ret;

        nif_data = enif_priv_data(env);

        if (argc != 3) {
                return enif_make_badarg(env);
        }

        if (esqlite_inspect_binary_string(env, argv[0], &path) == 0) {
                return enif_make_badarg(env);
        }

        if (esqlite_inspect_open_flags(env, argv[1], &flags) == 0) {
                return enif_make_badarg(env);
        }

        if (esqlite_is_atom(env, argv[2], "undefined")) {
                vfs = NULL;
        } else if (esqlite_inspect_binary_string(env, argv[2], &vfs) == 0) {
                return enif_make_badarg(env);
        }

        ret = sqlite3_open_v2(path, &db, flags, vfs);
        if (!db || ret != SQLITE_OK) {
                ERL_NIF_TERM reason;

                reason = esqlite_error_code(env, db);

                enif_free(path);
                enif_free(vfs);

                return esqlite_error_tuple(env, reason);
        }

        enif_free(path);
        enif_free(vfs);

        db_resource = enif_alloc_resource(nif_data->database_resource_type,
                                          sizeof(*db_resource));
        *db_resource = db;

        return esqlite_ok_tuple(env, enif_make_resource(env, db_resource));
}

ERL_NIF_TERM
esqlite_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct esqlite_nif_data *nif_data;
        struct sqlite3 **pdb, *db;
        int ret;

        nif_data = enif_priv_data(env);

        if (argc != 1) {
                return enif_make_badarg(env);
        }

        ret = enif_get_resource(env, argv[0], nif_data->database_resource_type,
                                (void **)&pdb);
        if (ret == 0) {
                return enif_make_badarg(env);
        }
        db = *pdb;

        ret = sqlite3_close_v2(db);

        return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
esqlite_error_code(ErlNifEnv *env, struct sqlite3 *db) {
        int code;

        code = sqlite3_extended_errcode(db);

        switch (code) {
        case SQLITE_ABORT:
                return enif_make_atom(env, "abort");
        case SQLITE_AUTH:
                return enif_make_atom(env, "auth");
        case SQLITE_BUSY:
                return enif_make_atom(env, "busy");
        case SQLITE_CANTOPEN:
                return enif_make_atom(env, "cantopen");
        case SQLITE_CONSTRAINT:
                return enif_make_atom(env, "constraint");
        case SQLITE_CORRUPT:
                return enif_make_atom(env, "corrupt");
        case SQLITE_DONE:
                return enif_make_atom(env, "done");
        case SQLITE_EMPTY:
                return enif_make_atom(env, "empty");
        case SQLITE_ERROR:
                return enif_make_atom(env, "error");
        case SQLITE_FORMAT:
                return enif_make_atom(env, "format");
        case SQLITE_FULL:
                return enif_make_atom(env, "full");
        case SQLITE_INTERNAL:
                return enif_make_atom(env, "internal");
        case SQLITE_INTERRUPT:
                return enif_make_atom(env, "interrupt");
        case SQLITE_IOERR:
                return enif_make_atom(env, "ioerr");
        case SQLITE_LOCKED:
                return enif_make_atom(env, "locked");
        case SQLITE_MISMATCH:
                return enif_make_atom(env, "mismatch");
        case SQLITE_MISUSE:
                return enif_make_atom(env, "misuse");
        case SQLITE_NOLFS:
                return enif_make_atom(env, "nolfs");
        case SQLITE_NOMEM:
                return enif_make_atom(env, "nomem");
        case SQLITE_NOTADB:
                return enif_make_atom(env, "notadb");
        case SQLITE_NOTFOUND:
                return enif_make_atom(env, "notfound");
        case SQLITE_NOTICE:
                return enif_make_atom(env, "notice");
        case SQLITE_OK:
                return enif_make_atom(env, "ok");
        case SQLITE_PERM:
                return enif_make_atom(env, "perm");
        case SQLITE_PROTOCOL:
                return enif_make_atom(env, "protocol");
        case SQLITE_RANGE:
                return enif_make_atom(env, "range");
        case SQLITE_READONLY:
                return enif_make_atom(env, "readonly");
        case SQLITE_ROW:
                return enif_make_atom(env, "row");
        case SQLITE_SCHEMA:
                return enif_make_atom(env, "schema");
        case SQLITE_TOOBIG:
                return enif_make_atom(env, "toobig");
        case SQLITE_WARNING:
                return enif_make_atom(env, "warning");

        case SQLITE_ABORT_ROLLBACK:
                return enif_make_atom(env, "abort_rollback");
        case SQLITE_BUSY_RECOVERY:
                return enif_make_atom(env, "busy_recovery");
        case SQLITE_BUSY_SNAPSHOT:
                return enif_make_atom(env, "busy_snapshot");
        case SQLITE_BUSY_TIMEOUT:
                return enif_make_atom(env, "busy_timeout");
        case SQLITE_CANTOPEN_CONVPATH:
                return enif_make_atom(env, "cantopen_convpath");
        case SQLITE_CANTOPEN_DIRTYWAL:
                return enif_make_atom(env, "cantopen_dirtywal");
        case SQLITE_CANTOPEN_FULLPATH:
                return enif_make_atom(env, "cantopen_fullpath");
        case SQLITE_CANTOPEN_ISDIR:
                return enif_make_atom(env, "cantopen_isdir");
        case SQLITE_CANTOPEN_NOTEMPDIR:
                return enif_make_atom(env, "cantopen_notempdir");
        case SQLITE_CANTOPEN_SYMLINK:
                return enif_make_atom(env, "cantopen_symlink");
        case SQLITE_CONSTRAINT_CHECK:
                return enif_make_atom(env, "constraint_check");
        case SQLITE_CONSTRAINT_COMMITHOOK:
                return enif_make_atom(env, "constraint_commithook");
        case SQLITE_CONSTRAINT_FOREIGNKEY:
                return enif_make_atom(env, "constraint_foreignkey");
        case SQLITE_CONSTRAINT_FUNCTION:
                return enif_make_atom(env, "constraint_function");
        case SQLITE_CONSTRAINT_NOTNULL:
                return enif_make_atom(env, "constraint_notnull");
        case SQLITE_CONSTRAINT_PINNED:
                return enif_make_atom(env, "constraint_pinned");
        case SQLITE_CONSTRAINT_PRIMARYKEY:
                return enif_make_atom(env, "constraint_primarykey");
        case SQLITE_CONSTRAINT_ROWID:
                return enif_make_atom(env, "constraint_rowid");
        case SQLITE_CONSTRAINT_TRIGGER:
                return enif_make_atom(env, "constraint_trigger");
        case SQLITE_CONSTRAINT_UNIQUE:
                return enif_make_atom(env, "constraint_unique");
        case SQLITE_CONSTRAINT_VTAB:
                return enif_make_atom(env, "constraint_vtab");
        case SQLITE_CORRUPT_INDEX:
                return enif_make_atom(env, "corrupt_index");
        case SQLITE_CORRUPT_SEQUENCE:
                return enif_make_atom(env, "corrupt_sequence");
        case SQLITE_CORRUPT_VTAB:
                return enif_make_atom(env, "corrupt_vtab");
        case SQLITE_ERROR_MISSING_COLLSEQ:
                return enif_make_atom(env, "error_missing_collseq");
        case SQLITE_ERROR_RETRY:
                return enif_make_atom(env, "error_retry");
        case SQLITE_ERROR_SNAPSHOT:
                return enif_make_atom(env, "error_snapshot");
        case SQLITE_IOERR_ACCESS:
                return enif_make_atom(env, "ioerr_access");
        case SQLITE_IOERR_AUTH:
                return enif_make_atom(env, "ioerr_auth");
        case SQLITE_IOERR_BEGIN_ATOMIC:
                return enif_make_atom(env, "ioerr_begin_atomic");
        case SQLITE_IOERR_BLOCKED:
                return enif_make_atom(env, "ioerr_blocked");
        case SQLITE_IOERR_CHECKRESERVEDLOCK:
                return enif_make_atom(env, "ioerr_checkreservedlock");
        case SQLITE_IOERR_CLOSE:
                return enif_make_atom(env, "ioerr_close");
        case SQLITE_IOERR_COMMIT_ATOMIC:
                return enif_make_atom(env, "ioerr_commit_atomic");
        case SQLITE_IOERR_CONVPATH:
                return enif_make_atom(env, "ioerr_convpath");
        case SQLITE_IOERR_DATA:
                return enif_make_atom(env, "ioerr_data");
        case SQLITE_IOERR_DELETE:
                return enif_make_atom(env, "ioerr_delete");
        case SQLITE_IOERR_DELETE_NOENT:
                return enif_make_atom(env, "ioerr_delete_noent");
        case SQLITE_IOERR_DIR_CLOSE:
                return enif_make_atom(env, "ioerr_dir_close");
        case SQLITE_IOERR_DIR_FSYNC:
                return enif_make_atom(env, "ioerr_dir_fsync");
        case SQLITE_IOERR_FSTAT:
                return enif_make_atom(env, "ioerr_fstat");
        case SQLITE_IOERR_FSYNC:
                return enif_make_atom(env, "ioerr_fsync");
        case SQLITE_IOERR_GETTEMPPATH:
                return enif_make_atom(env, "ioerr_gettemppath");
        case SQLITE_IOERR_LOCK:
                return enif_make_atom(env, "ioerr_lock");
        case SQLITE_IOERR_MMAP:
                return enif_make_atom(env, "ioerr_mmap");
        case SQLITE_IOERR_NOMEM:
                return enif_make_atom(env, "ioerr_nomem");
        case SQLITE_IOERR_RDLOCK:
                return enif_make_atom(env, "ioerr_rdlock");
        case SQLITE_IOERR_READ:
                return enif_make_atom(env, "ioerr_read");
        case SQLITE_IOERR_ROLLBACK_ATOMIC:
                return enif_make_atom(env, "ioerr_rollback_atomic");
        case SQLITE_IOERR_SEEK:
                return enif_make_atom(env, "ioerr_seek");
        case SQLITE_IOERR_SHMLOCK:
                return enif_make_atom(env, "ioerr_shmlock");
        case SQLITE_IOERR_SHMMAP:
                return enif_make_atom(env, "ioerr_shmmap");
        case SQLITE_IOERR_SHMOPEN:
                return enif_make_atom(env, "ioerr_shmopen");
        case SQLITE_IOERR_SHMSIZE:
                return enif_make_atom(env, "ioerr_shmsize");
        }

        return enif_make_int(env, code);
}

static int
esqlite_inspect_open_flags(ErlNifEnv *env, ERL_NIF_TERM list, int *pflags) {
        ERL_NIF_TERM head, tail;
        int flags;

        if (!enif_is_list(env, list)) {
                return 0;
        }

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
