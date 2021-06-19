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
                   fun step/1]}
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
