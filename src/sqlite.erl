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

-export([open/1, open/2, open/3, close/1]).

-export_type([error_reason/0, result/0, result/1]).

-type error_reason() ::
        {open, sqlite_nif:error_code()}.

-type result() :: ok | {error, error_reason()}.
-type result(Result) :: {ok, Result} | {error, error_reason()}.

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
