%%%
%%% mpln_misc_run: miscellaneous runtime functions
%%%
%%% Copyright (c) 2011 Megaplan Ltd. (Russia)
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom
%%% the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% @author arkdro <arkdro@gmail.com>
%%% @since 2011-07-15 10:00
%%% @license MIT
%%% @doc miscellaneous runtime functions
%%%

-module(mpln_misc_run).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([write_pid/1, remove_pid/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc creates dir for pid file, writes erlang VM pid to file
%%
-spec write_pid(string()) -> ok | {error, any()}.

write_pid(File) ->
    case filelib:ensure_dir(File) of
        ok ->
            write_file(File);
        {error, Reason} ->
            error_logger:info_report({?MODULE, write_pid_error, ?LINE, Reason}),
            {error, Reason}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc removes pid file
%%
-spec remove_pid(string()) -> ok | {error, any()}.

remove_pid(File) ->
    file:delete(File).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc writes erlang VM pid to file
%%
write_file(File) ->
    Pid = os:getpid(),
    Data = list_to_binary(Pid ++ "\n"),
    case file:write_file(File, Data) of
        ok ->
            ok;
        {error, Reason} ->
            error_logger:info_report({?MODULE, write_file_error, ?LINE, Reason}),
            {error, Reason}
    end.
