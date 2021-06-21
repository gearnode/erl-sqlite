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
