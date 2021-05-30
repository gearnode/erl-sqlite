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

-module(sqlite_nif).

-export([libversion/0, sourceid/0]).

-on_load(init/0).

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
