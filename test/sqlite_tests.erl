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

-module(sqlite_tests).

-include_lib("eunit/include/eunit.hrl").

sqlite_test_() ->
  {setup,
   fun () ->
       {ok, Db} = sqlite:open(<<":memory:">>),
       Db
   end,
   fun (Db) ->
       sqlite:close(Db)
   end,
   fun (Db) ->
       {with, Db, [fun query/1,
                   fun with_transaction_ok/1,
                   fun with_transaction_ok_value/1,
                   fun with_transaction_error_value/1,
                   fun with_transaction_throw/1,
                   fun with_transaction_error/1,
                   fun with_transaction_exit/1,
                   fun with_transaction_commit_failure/1]}
   end}.

database_open_error_test() ->
  %% We cannot use sqlite:open/2 because it uses gen_server:start_link/3,
  %% which causes an EXIT signal when initialization fails, which causes eunit
  %% to freak out. Until we ditch eunit, there is nothing we can do about it.
  ?assertEqual({error, {open, cantopen}},
               gen_server:start(sqlite_database, ["/does_not_exist", #{}],
                                [])).

database_test() ->
  ?assertMatch({ok, _}, sqlite:open({local, test_db}, ":memory:", #{})),
  sqlite:close(test_db).

query(Db) ->
  Query = fun (Query, Parameters) ->
              sqlite:query(Db, Query, Parameters)
          end,
  Schema = ["CREATE TABLE products",
            "  (id INTEGER PRIMARY KEY,"
            "   name TEXT NOT NULL,",
            "   count INTEGER NOT NULL,"
            "   price REAL NOT NULL,",
            "   data BLOB)"],
  ?assertEqual({ok, []}, Query(Schema, [])),
  ?assertEqual({ok, []},
               Query("INSERT INTO products"
                     "    (id, name, count, price, data)"
                     "  VALUES (?1, ?2, ?3, ?4, ?5)",
                     [1, <<"foo">>, 5, 50.0, null])),
  ?assertEqual({ok, []},
               Query("INSERT INTO products"
                     "    (id, name, count, price, data)"
                     "  VALUES (?1, ?2, ?3, ?4, ?5)",
                     [2, <<"bar">>, 20, 200.0, <<1,2,3>>])),
  ?assertEqual({ok, []},
               Query("SELECT * FROM products WHERE id < 1", [])),
  ?assertEqual({ok, [[1, 5], [2, 20]]},
               Query("SELECT id, count FROM products ORDER BY id", [])),
  ?assertEqual({ok, [[250.0]]},
               Query("SELECT SUM(price) FROM products", [])),
  ?assertEqual({ok, [[1, <<"foo">>, null],
                     [2, <<"bar">>, <<1,2,3>>]]},
               Query("SELECT id, name, data FROM products", [])).

with_transaction_ok(Db) ->
  Table = random_table_name(),
  F = fun (_) -> create_table(Db, Table) end,
  ?assertEqual(ok, sqlite:with_transaction(Db, F)),
  ?assert(table_exists(Db, Table)).

with_transaction_ok_value(Db) ->
  Table = random_table_name(),
  F = fun (_) -> create_table(Db, Table), {ok, foo} end,
  ?assertEqual({ok, foo}, sqlite:with_transaction(Db, F)),
  ?assert(table_exists(Db, Table)).

with_transaction_error_value(Db) ->
  Table = random_table_name(),
  F = fun (_) -> create_table(Db, Table), {error, foo} end,
  ?assertEqual({error, foo}, sqlite:with_transaction(Db, F)),
  ?assertNot(table_exists(Db, Table)).

with_transaction_throw(Db) ->
  Table = random_table_name(),
  F = fun (_) -> create_table(Db, Table), throw(foo) end,
  ?assertThrow(foo, sqlite:with_transaction(Db, F)),
  ?assertNot(table_exists(Db, Table)).

with_transaction_error(Db) ->
  Table = random_table_name(),
  F = fun (_) -> create_table(Db, Table), error(foo) end,
  ?assertError(foo, sqlite:with_transaction(Db, F)),
  ?assertNot(table_exists(Db, Table)).

with_transaction_exit(Db) ->
  Table = random_table_name(),
  F = fun (_) -> create_table(Db, Table), exit(foo) end,
  ?assertExit(foo, sqlite:with_transaction(Db, F)),
  ?assertNot(table_exists(Db, Table)).

with_transaction_commit_failure(Db) ->
  Table1 = random_table_name(),
  Table2 = random_table_name(),
  Query = fun (Q) -> {ok, _} = sqlite:query(Db, Q) end,
  Query("PRAGMA foreign_keys = ON"),
  F = fun (_) ->
          Query(["CREATE TABLE ", Table1,
                 "  (i INTEGER PRIMARY KEY)"]),
          Query(["CREATE TABLE ", Table2,
                 "  (j INTEGER REFERENCES ", Table1, " (i)",
                 "   DEFERRABLE INITIALLY DEFERRED)"]),
          Query(["INSERT INTO ", Table1, " (i) VALUES (1)"]),
          Query(["INSERT INTO ", Table2, " (j) VALUES (2)"])
      end,
  ?assertEqual({error, {commit, {step, constraint_foreignkey}}},
               sqlite:with_transaction(Db, F)),
  ?assertNot(table_exists(Db, Table1)),
  ?assertNot(table_exists(Db, Table2)).

-spec random_table_name() -> unicode:chardata().
random_table_name() ->
  Now = os:system_time(nanosecond),
  sqlite_utils:binary(io_lib:format("test_~b", [Now])).

-spec create_table(sqlite_database:ref(), unicode:chardata()) -> ok.
create_table(Db, Name) ->
  Query = ["CREATE TABLE ", Name, " (i INTEGER)"],
  {ok, _} = sqlite:query(Db, Query),
  ok.

-spec table_exists(sqlite_database:ref(), unicode:chardata()) -> boolean().
table_exists(Db, Name) ->
  Query = ["SELECT COUNT(*)",
           "  FROM sqlite_master",
           "  WHERE type = 'table' AND name = ?"],
  {ok, [[Count]]} = sqlite:query(Db, Query, [Name]),
  Count == 1.
