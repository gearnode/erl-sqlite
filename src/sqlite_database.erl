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

-module(sqlite_database).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/2, start_link/3, stop/1]).
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

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Path0, Options]) ->
  logger:update_process_metadata(#{domain => [sqlite, database]}),
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
