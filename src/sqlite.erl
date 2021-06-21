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

-module(sqlite).

-export([open/1, open/2, open/3, close/1,
         query/3, query/4]).

-export_type([error_reason/0, result/0, result/1,
              query_options/0, query/0, parameter/0, row/0, column/0]).

-type error_reason() ::
        {open, sqlite_nif:error_code()}
      | {prepare, sqlite_nif:error_code()}
      | {bind, sqlite_nif:error_code(), pos_integer(), parameter()}
      | {step, sqlite_nif:error_code()}
      | {invalid_parameter, pos_integer(), parameter()}.

-type result() :: ok | {error, error_reason()}.
-type result(Result) :: {ok, Result} | {error, error_reason()}.

-type query_options() ::
        #{}.

-type query() :: unicode:chardata().
-type parameter() ::
        null
      | {integer, integer()}
      | integer()
      | {float, float()}
      | float()
      | {blob, binary()}
      | {text, binary()}
      | binary().

-type row() :: [column()].
-type column() :: term().

-spec open(unicode:chardata()) -> result(pid()).
open(Path) ->
  open(Path, #{}).

-spec open(unicode:chardata(), sqlite_database:options()) -> result(pid()).
open(Path, Options) ->
  sqlite_database:start_link(Path, Options).

-spec open(sqlite_database:name(), unicode:chardata(),
           sqlite_database:options()) ->
        result(pid()).
open(Name, Path, Options) ->
  sqlite_database:start_link(Name, Path, Options).

-spec close(sqlite_database:ref()) -> ok.
close(Ref) ->
  sqlite_database:stop(Ref).

-spec query(sqlite_database:ref(), query(), [parameter()]) ->
        result({[row()], query()}).
query(Database, Query, Parameters) ->
  query(Database, Query, Parameters, #{}).

-spec query(sqlite_database:ref(), query(), [parameter()], query_options()) ->
        result({[row()], query()}).
query(Database, Query, Parameters, Options) ->
  sqlite_database:call(Database, {query, Query, Parameters, Options}).
