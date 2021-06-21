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
       {with, Db, [fun query/1]}
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
  ?assertMatch({ok, _}, Query(Schema, [])),
  ?assertMatch({ok, _},
               Query("INSERT INTO products"
                     "    (id, name, count, price, data)"
                     "  VALUES (?1, ?2, ?3, ?4, ?5)",
                     [1, <<"foo">>, 5, 50.0, null])),
  ?assertMatch({ok, _},
               Query("INSERT INTO products"
                     "    (id, name, count, price, data)"
                     "  VALUES (?1, ?2, ?3, ?4, ?5)",
                     [2, <<"bar">>, 20, 200.0, <<1,2,3>>])),
  ?assertEqual({ok, {[], <<>>}},
               Query("SELECT * FROM products WHERE id < 1", [])),
  ?assertEqual({ok, {[[1, 5], [2, 20]], <<>>}},
               Query("SELECT id, count FROM products ORDER BY id", [])),
  ?assertEqual({ok, {[[250.0]], <<>>}},
               Query("SELECT SUM(price) FROM products", [])),
  ?assertEqual({ok, {[[1, <<"foo">>, null],
                      [2, <<"bar">>, <<1,2,3>>]], <<>>}},
               Query("SELECT id, name, data FROM products", [])).
