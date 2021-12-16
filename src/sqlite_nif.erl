%% Copyright (c) 2021 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(sqlite_nif).

-export([libversion/0, sourceid/0,
         open/3, close/1,
         prepare/3, finalize/1, step/1, reset/1,
         column_count/1, column_type/2, column_bytes/2,
         column_blob/2, column_double/2, column_int64/2, column_text/2,
         bind_blob64/3, bind_double/3, bind_int64/3, bind_null/2,
         bind_text64/3]).

-export_type([database/0, statement/0,
              result/0, result/1,
              result_code/0, non_error_result_code/0, error_code/0,
              primary_error_code/0, extended_error_code/0,
              open_flag/0, prepare_flag/0,
              datatype/0]).

-on_load(init/0).

-type database() :: reference().
-type statement() :: reference().

-type result() :: ok | {error, error_code()}.
-type result(Result) :: {ok, Result} | {error, error_code()}.

-type result_code() ::
        non_error_result_code()
      | error_code().

-type non_error_result_code() ::
        ok
      | row
      | done.

-type error_code() ::
        primary_error_code()
      | extended_error_code()
      | integer().

-type primary_error_code() ::
        abort
      | auth
      | busy
      | cantopen
      | constraint
      | corrupt
      | done
      | empty
      | error
      | format
      | full
      | internal
      | interrupt
      | ioerr
      | locked
      | mismatch
      | misuse
      | nolfs
      | nomem
      | notadb
      | notfound
      | notice
      | ok
      | perm
      | protocol
      | range
      | readonly
      | row
      | schema
      | toobig
      | warning.

-type extended_error_code() ::
        abort_rollback
      | busy_recovery
      | busy_snapshot
      | busy_timeout
      | cantopen_convpath
      | cantopen_dirtywal
      | cantopen_fullpath
      | cantopen_isdir
      | cantopen_notempdir
      | cantopen_symlink
      | constraint_check
      | constraint_commithook
      | constraint_foreignkey
      | constraint_function
      | constraint_notnull
      | constraint_pinned
      | constraint_primarykey
      | constraint_rowid
      | constraint_trigger
      | constraint_unique
      | constraint_vtab
      | corrupt_index
      | corrupt_sequence
      | corrupt_vtab
      | error_missing_collseq
      | error_retry
      | error_snapshot
      | ioerr_access
      | ioerr_auth
      | ioerr_begin_atomic
      | ioerr_blocked
      | ioerr_checkreservedlock
      | ioerr_close
      | ioerr_commit_atomic
      | ioerr_convpath
      | ioerr_data
      | ioerr_delete
      | ioerr_delete_noent
      | ioerr_dir_close
      | ioerr_dir_fsync
      | ioerr_fstat
      | ioerr_fsync
      | ioerr_gettemppath
      | ioerr_lock
      | ioerr_mmap
      | ioerr_nomem
      | ioerr_rdlock
      | ioerr_read
      | ioerr_rollback_atomic
      | ioerr_seek
      | ioerr_shmlock
      | ioerr_shmmap
      | ioerr_shmopen
      | ioerr_shmsize
      | ioerr_short_read
      | ioerr_truncate
      | ioerr_unlock
      | ioerr_vnode
      | ioerr_write
      | locked_sharedcache
      | locked_vtab
      | notice_recover_rollback
      | notice_recover_wal
      | ok_load_permanently
      | readonly_cantinit
      | readonly_cantlock
      | readonly_dbmoved
      | readonly_directory
      | readonly_recovery
      | readonly_rollback
      | warning_autoindex.

-type open_flag() ::
        readonly
      | readwrite
      | create
      | uri
      | memory
      | nomutex
      | fullmutex
      | sharedcache
      | privatecache
      | nofollow.

-type prepare_flag() ::
        persistent
      | normalize
      | no_vtab.

-type datatype() ::
        blob
      | float
      | integer
      | null
      | text.

init() ->
  Path = filename:join(find_nif_directory(sqlite), "sqlite_nif"),
  erlang:load_nif(Path, []).

-spec find_nif_directory(AppName :: atom()) -> string().
find_nif_directory(AppName) ->
  PrivDir = code:priv_dir(AppName),
  case filelib:is_dir(PrivDir) of
    true ->
      %% If the private directory exists, we are in a release and the library
      %% is directly there.
      PrivDir;
    false ->
      %% If the private directory does not exists, we (probably) are in an
      %% escript.
      Root = escript_directory(AppName),
      filename:join([Root, "lib", AppName, "priv"])
  end.

-spec escript_directory(AppName :: atom()) -> string().
escript_directory(AppName) ->
  %% When the application is packaged into an escript, code:lib_dir/1 returns
  %% <app>/sqlite where <app> is the name of the escript (no it does not make
  %% any sense since it is a file and not a directory, but there is nothing I
  %% can do about it). Given this path, we can obtain the directory containing
  %% the escript itself.
  %%
  %% There is no standard way to package an escript, so we will assume that it
  %% is part of a "bin" directory, and that NIF shared libraries are stored in
  %% a lib/sqlite/priv directory.
  %%
  %% This happens to match the way rebar3 organize the _build directory.
  LibDir = code:lib_dir(AppName),
  case lists:reverse(filename:split(LibDir)) of
    [_ | [_ | [_ | Parts]]] ->
      filename:join(lists:reverse(Parts));
    _ ->
      %% If we end up here, then the sqlite module was packaged in a different
      %% way. Nothing else we can do.
      error({invalid_lib_dir, LibDir})
  end.

-spec libversion() -> binary().
libversion() ->
  erlang:nif_error(nif_not_loaded).

-spec sourceid() -> binary().
sourceid() ->
  erlang:nif_error(nif_not_loaded).

-spec open(binary(), [open_flag()], binary() | undefined) ->
        result(database()).
open(_Path, _Flags, _MaybeVfs) ->
  erlang:nif_error(nif_not_loaded).

-spec close(database()) -> ok.
close(_Db) ->
  erlang:nif_error(nif_not_loaded).

-spec prepare(database(), binary(), [prepare_flag()]) ->
        result({statement(), binary()}).
prepare(_Db, _Query, _Flags) ->
  erlang:nif_error(nif_not_loaded).

-spec finalize(statement()) -> ok.
finalize(_Stmt) ->
  erlang:nif_error(nif_not_loaded).

-spec step(statement()) -> result(row | done).
step(_Stmt) ->
  erlang:nif_error(nif_not_loaded).

-spec reset(statement()) -> result().
reset(_Stmt) ->
  erlang:nif_error(nif_not_loaded).

-spec column_count(statement()) -> non_neg_integer().
column_count(_Stmt) ->
  erlang:nif_error(nif_not_loaded).

-spec column_type(statement(), non_neg_integer()) -> datatype().
column_type(_Stmt, _Column) ->
  erlang:nif_error(nif_not_loaded).

-spec column_bytes(statement(), non_neg_integer()) -> non_neg_integer().
column_bytes(_Stmt, _Column) ->
  erlang:nif_error(nif_not_loaded).

-spec column_blob(statement(), non_neg_integer()) -> binary().
column_blob(_Stmt, _Column) ->
  erlang:nif_error(nif_not_loaded).

-spec column_double(statement(), non_neg_integer()) -> float().
column_double(_Stmt, _Column) ->
  erlang:nif_error(nif_not_loaded).

-spec column_int64(statement(), non_neg_integer()) -> integer().
column_int64(_Stmt, _Column) ->
  erlang:nif_error(nif_not_loaded).

-spec column_text(statement(), non_neg_integer()) -> binary().
column_text(_Stmt, _Column) ->
  erlang:nif_error(nif_not_loaded).

-spec bind_blob64(statement(), pos_integer(), binary()) -> result().
bind_blob64(_Stmt, _Parameter, _Value) ->
  erlang:nif_error(nif_not_loaded).

-spec bind_double(statement(), pos_integer(), float()) -> result().
bind_double(_Stmt, _Parameter, _Value) ->
  erlang:nif_error(nif_not_loaded).

-spec bind_int64(statement(), pos_integer(), integer()) -> result().
bind_int64(_Stmt, _Parameter, _Value) ->
  erlang:nif_error(nif_not_loaded).

-spec bind_null(statement(), pos_integer()) -> result().
bind_null(_Stmt, _Parameter) ->
  erlang:nif_error(nif_not_loaded).

-spec bind_text64(statement(), pos_integer(), binary()) -> result().
bind_text64(_Stmt, _Parameter, _Value) ->
  erlang:nif_error(nif_not_loaded).
