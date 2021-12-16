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

-module(sqlite_database).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/2, start_link/3, stop/1, call/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([name/0, ref/0, options/0]).

-type name() :: et_gen_server:name().
-type ref() :: et_gen_server:ref().

-type options() :: #{open_flags => [sqlite_nif:open_flag()]}.

-type state() :: #{options := options(),
                   database := sqlite_nif:database()}.

-spec start_link(unicode:chardata(), options()) ->
        et_gen_server:start_ret().
start_link(Path, Options) ->
  gen_server:start_link(?MODULE, [Path, Options], []).

-spec start_link(name(), unicode:chardata(), options()) ->
        et_gen_server:start_ret().
start_link(Name, Path, Options) ->
  gen_server:start_link(Name, ?MODULE, [Path, Options], []).

-spec stop(ref()) -> ok.
stop(Ref) ->
  gen_server:stop(Ref).

-spec call(ref(), term()) -> sqlite:result(term()).
call(Ref, Message) ->
  gen_server:call(Ref, Message, infinity).

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Path0, Options]) ->
  logger:update_process_metadata(#{domain => [sqlite, database]}),
  process_flag(trap_exit, true), % terminate/2 must always be called
  Path = sqlite_utils:binary(Path0),
  Flags = maps:get(open_flags, Options, [readwrite, create]),
  case sqlite_nif:open(Path, Flags, undefined) of
    {ok, Database} ->
      State = #{options => Options,
                database => Database},
      {ok, State};
    {error, Code} ->
      {stop, {open, Code}}
  end.

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(_Reason, #{database := Database}) ->
  sqlite_nif:close(Database),
  ok.

-spec handle_call(term(), {pid(), et_gen_server:request_id()}, state()) ->
        et_gen_server:handle_call_ret(state()).
handle_call({query, Query, Parameters, Options}, _From, State) ->
  {Result, State} = query(Query, Parameters, Options, State),
  {reply, Result, State};
handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> et_gen_server:handle_cast_ret(state()).
handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec handle_info(term(), state()) -> et_gen_server:handle_info_ret(state()).
handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec query(sqlite:query(), [sqlite:parameter()], sqlite:query_options(),
            state()) ->
        {sqlite:result([sqlite:row()]), state()}.
query(Query0, Parameters, Options, State = #{database := Database}) ->
  Flags = [],
  Query = sqlite_utils:binary(Query0),
  case sqlite_nif:prepare(Database, Query, Flags) of
    {ok, {Statement, _}} ->
      try
        query_bind(Statement, 1, Parameters, Options, State)
      after
        sqlite_nif:finalize(Statement)
      end;
    {error, Code} ->
      {{error, {prepare, Code}}, State}
  end.

-spec query_bind(sqlite_nif:statement(), pos_integer(), [sqlite:parameter()],
             sqlite:query_options(), state()) ->
        {sqlite:result([sqlite:row()]), state()}.
query_bind(Statement, _N, [], Options, State) ->
  query_step(Statement, [], Options, State);
query_bind(Statement, N, [Parameter | Parameters], Options, State) ->
  case bind(Statement, N, Parameter) of
    ok ->
      query_bind(Statement, N+1, Parameters, Options, State);
    {error, Code} ->
      {{error, {bind, Code, N, Parameter}}, State}
  end.

-spec bind(sqlite_nif:statement(), pos_integer(), sqlite:parameter()) ->
        sqlite:result().
bind(Statement, N, null) ->
  sqlite_nif:bind_null(Statement, N);
bind(Statement, N, Parameter) when is_integer(Parameter) ->
  bind(Statement, N, {integer, Parameter});
bind(Statement, N, Parameter) when is_float(Parameter) ->
  bind(Statement, N, {float, Parameter});
bind(Statement, N, Parameter) when is_binary(Parameter) ->
  bind(Statement, N, {text, Parameter});
bind(Statement, N, {integer, Parameter}) ->
  sqlite_nif:bind_int64(Statement, N, Parameter);
bind(Statement, N, {float, Parameter}) ->
  sqlite_nif:bind_double(Statement, N, Parameter);
bind(Statement, N, {blob, Parameter}) ->
  sqlite_nif:bind_blob64(Statement, N, Parameter);
bind(Statement, N, {text, Parameter}) ->
  sqlite_nif:bind_text64(Statement, N, Parameter);
bind(_Statement, N, Parameter) ->
  {error, {invalid_parameter, N, Parameter}}.

-spec query_step(sqlite_nif:statement(), [sqlite:row()],
                 sqlite:query_options(), state()) ->
        {sqlite:result([sqlite:row()]), state()}.
query_step(Statement, Rows, Options, State) ->
  case sqlite_nif:step(Statement) of
    {ok, row} ->
      query_step(Statement, [row(Statement) | Rows], Options, State);
    {ok, done} ->
      {{ok, lists:reverse(Rows)}, State};
    {error, Code} ->
      {{error, {step, Code}}, State}
  end.

-spec row(sqlite_nif:statement()) -> sqlite:row().
row(Statement) ->
  NbColumns = sqlite_nif:column_count(Statement),
  [column(Statement, N) || N <- lists:seq(0, NbColumns-1)].

-spec column(sqlite_nif:statement(), non_neg_integer()) -> sqlite:column().
column(Statement, N) ->
  case sqlite_nif:column_type(Statement, N) of
    null ->
      null;
    integer ->
      sqlite_nif:column_int64(Statement, N);
    float ->
      sqlite_nif:column_double(Statement, N);
    blob ->
      sqlite_nif:column_blob(Statement, N);
    text ->
      sqlite_nif:column_text(Statement, N)
  end.
