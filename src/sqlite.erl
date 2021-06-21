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
         query/2, query/3, query/4,
         with_transaction/2, with_transaction/3]).

-export_type([error_reason/0, rollback_original_error_reason/0,
              result/0, result/1,
              query_options/0, query/0, parameter/0, row/0, column/0,
              transaction_behaviour/0, transaction_fun/0,
              transaction_result/0]).

-type error_reason() ::
        {open, sqlite_nif:error_code()}
      | {prepare, sqlite_nif:error_code()}
      | {bind, sqlite_nif:error_code(), pos_integer(), parameter()}
      | {step, sqlite_nif:error_code()}
      | {invalid_parameter, pos_integer(), parameter()}
      | {'begin', error_reason()}
      | {commit, error_reason()}
      | {rollback, error_reason(), rollback_original_error_reason()}.

-type rollback_original_error_reason() ::
        error_reason()
      | {throw, term()}
      | {error, term()}
      | {exit, term()}.

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

-type transaction_behaviour() ::
        deferred
      | immediate
      | exclusive.

-type transaction_fun() ::
        fun((sqlite_database:ref()) -> transaction_result()).

-type transaction_result() ::
        ok | {ok, term()} | {error, term()}.

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
close(Database) ->
  sqlite_database:stop(Database).

-spec query(sqlite_database:ref(), query()) ->
        result({[row()], query()}).
query(Database, Query) ->
  query(Database, Query, [], #{}).

-spec query(sqlite_database:ref(), query(), [parameter()]) ->
        result({[row()], query()}).
query(Database, Query, Parameters) ->
  query(Database, Query, Parameters, #{}).

-spec query(sqlite_database:ref(), query(), [parameter()], query_options()) ->
        result({[row()], query()}).
query(Database, Query, Parameters, Options) ->
  sqlite_database:call(Database, {query, Query, Parameters, Options}).

-spec with_transaction(sqlite_database:ref(), transaction_fun()) ->
        transaction_result().
with_transaction(Database, Fun) ->
  with_transaction(Database, Fun, deferred).

-spec with_transaction(sqlite_database:ref(), transaction_fun(),
                       transaction_behaviour()) ->
        transaction_result().
with_transaction(Database, Fun, Behaviour) ->
  try
    begin_transaction(Database, Behaviour),
    case Fun(Database) of
      ok ->
        commit(Database);
      {ok, FunResult} ->
        commit(Database),
        {ok, FunResult};
      {error, FunReason} ->
        rollback(Database, FunReason),
        {error, FunReason}
    end
  catch
    throw:(BeginReason = {'begin', _}) ->
      {error, BeginReason};
    throw:(CommitReason = {commit, _}) ->
      try
        rollback(Database, CommitReason)
      catch
        throw:(RollbackReason = {rollback, _, _}) ->
          {error, RollbackReason}
      end,
      {error, CommitReason};
    throw:(RollbackReason = {rollback, _, _}) ->
      {error, RollbackReason};
    Type:Reason:Trace ->
      try
        rollback(Database, {Type, Reason})
      catch
        throw:(RollbackReason = {rollback, _, _}) ->
          {error, RollbackReason}
      end,
      erlang:raise(Type, Reason, Trace)
  end.

-spec begin_transaction(sqlite_database:ref(), transaction_behaviour()) ->
        result().
begin_transaction(Database, Behaviour) ->
  Query = case Behaviour of
            deferred ->
              <<"BEGIN DEFERRED">>;
            immediate ->
              <<"BEGIN IMMEDIATE">>;
            exclusive ->
              <<"BEGIN EXCLUSIVE">>
          end,
  case query(Database, Query, []) of
    {ok, {_, _}} ->
      ok;
    {error, Reason} ->
      throw({'begin', Reason})
  end.

-spec commit(sqlite_database:ref()) -> ok.
commit(Database) ->
  case query(Database, <<"COMMIT">>, []) of
    {ok, {_, _}} ->
      ok;
    {error, Reason} ->
      throw({commit, Reason})
  end.

-spec rollback(sqlite_database:ref(), error_reason()) -> ok.
rollback(Database, OriginalErrorReason) ->
  case query(Database, <<"ROLLBACK">>, []) of
    {ok, {_, _}} ->
      ok;
    {error, Reason} ->
      throw({rollback, Reason, OriginalErrorReason})
  end.
