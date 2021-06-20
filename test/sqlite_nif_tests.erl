%% Copyright (c) 2021 Nicolas Martyanoff <khaelin@gmail.com>.
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

-module(sqlite_nif_tests).

-include_lib("eunit/include/eunit.hrl").

nif_test_() ->
  {setup,
   fun () ->
       DbPath = <<"/tmp/erl-sqlite-test.db">>,
       {ok, Db} = sqlite_nif:open(DbPath, [readwrite, create], undefined),
       Db
   end,
   fun (Db) ->
       sqlite_nif:close(Db)
   end,
   fun (Db) ->
       {with, Db, [fun prepare/1,
                   fun step/1,
                   fun columns/1,
                   fun empty_columns/1,
                   fun bind/1]}
   end}.

prepare(Db) ->
  Query1 = <<"SELECT 1; SELECT 2">>,
  {ok, Stmt1, Query2} = sqlite_nif:prepare(Db, Query1, []),
  sqlite_nif:finalize(Stmt1),
  ?assertEqual(<<" SELECT 2">>, Query2),
  {ok, Stmt2, <<"">>} = sqlite_nif:prepare(Db, Query2, []),
  sqlite_nif:finalize(Stmt2).

step(Db) ->
  Query = <<"SELECT 1">>,
  {ok, Stmt, _} = sqlite_nif:prepare(Db, Query, []),
  ?assertEqual({ok, row}, sqlite_nif:step(Stmt)),
  ?assertEqual({ok, done}, sqlite_nif:step(Stmt)),
  ?assertEqual(ok, sqlite_nif:reset(Stmt)),
  ?assertEqual({ok, row}, sqlite_nif:step(Stmt)),
  ?assertEqual({ok, done}, sqlite_nif:step(Stmt)),
  sqlite_nif:finalize(Stmt).

columns(Db) ->
  Query = <<"SELECT NULL, 42, 3.14, 'foobar', X'616263'">>,
  {ok, Stmt, _} = sqlite_nif:prepare(Db, Query, []),
  ?assertEqual({ok, row}, sqlite_nif:step(Stmt)),
  ?assertEqual(5, sqlite_nif:column_count(Stmt)),
  ?assertEqual(null, sqlite_nif:column_type(Stmt, 0)),
  ?assertEqual(integer, sqlite_nif:column_type(Stmt, 1)),
  ?assertEqual(42, sqlite_nif:column_int64(Stmt, 1)),
  ?assertEqual(float, sqlite_nif:column_type(Stmt, 2)),
  ?assertEqual(3.14, sqlite_nif:column_double(Stmt, 2)),
  ?assertEqual(text, sqlite_nif:column_type(Stmt, 3)),
  ?assertEqual(6, sqlite_nif:column_bytes(Stmt, 3)),
  ?assertEqual(<<"foobar">>, sqlite_nif:column_text(Stmt, 3)),
  ?assertEqual(blob, sqlite_nif:column_type(Stmt, 4)),
  ?assertEqual(3, sqlite_nif:column_bytes(Stmt, 4)),
  ?assertEqual(<<97, 98, 99>>, sqlite_nif:column_blob(Stmt, 4)),
  sqlite_nif:finalize(Stmt).

empty_columns(Db) ->
  Query = <<"SELECT '', X''">>,
  {ok, Stmt, _} = sqlite_nif:prepare(Db, Query, []),
  ?assertEqual({ok, row}, sqlite_nif:step(Stmt)),
  ?assertEqual(2, sqlite_nif:column_count(Stmt)),
  ?assertEqual(text, sqlite_nif:column_type(Stmt, 0)),
  ?assertEqual(0, sqlite_nif:column_bytes(Stmt, 0)),
  ?assertEqual(<<"">>, sqlite_nif:column_text(Stmt, 0)),
  ?assertEqual(blob, sqlite_nif:column_type(Stmt, 1)),
  ?assertEqual(0, sqlite_nif:column_bytes(Stmt, 1)),
  ?assertEqual(<<>>, sqlite_nif:column_blob(Stmt, 1)),
  sqlite_nif:finalize(Stmt).

bind(Db) ->
  Query = <<"SELECT ?1, ?2, ?3, ?4, ?5, ?6, ?7">>,
  {ok, Stmt, _} = sqlite_nif:prepare(Db, Query, []),
  ?assertEqual(ok, sqlite_nif:bind_null(Stmt, 1)),
  ?assertEqual(ok, sqlite_nif:bind_int64(Stmt, 2, 42)),
  ?assertEqual(ok, sqlite_nif:bind_double(Stmt, 3, 3.14)),
  ?assertEqual(ok, sqlite_nif:bind_text64(Stmt, 4, <<"">>)),
  ?assertEqual(ok, sqlite_nif:bind_text64(Stmt, 5, <<"foobar">>)),
  ?assertEqual(ok, sqlite_nif:bind_blob64(Stmt, 6, <<>>)),
  ?assertEqual(ok, sqlite_nif:bind_blob64(Stmt, 7, <<97, 98, 99>>)),
  ?assertEqual({ok, row}, sqlite_nif:step(Stmt)),
  ?assertEqual(null, sqlite_nif:column_type(Stmt, 0)),
  ?assertEqual(42, sqlite_nif:column_int64(Stmt, 1)),
  ?assertEqual(3.14, sqlite_nif:column_double(Stmt, 2)),
  ?assertEqual(<<"">>, sqlite_nif:column_text(Stmt, 3)),
  ?assertEqual(<<"foobar">>, sqlite_nif:column_text(Stmt, 4)),
  ?assertEqual(<<>>, sqlite_nif:column_blob(Stmt, 5)),
  ?assertEqual(<<97, 98, 99>>, sqlite_nif:column_blob(Stmt, 6)),
  sqlite_nif:finalize(Stmt).
