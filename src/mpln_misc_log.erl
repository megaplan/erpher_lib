%%%
%%% mpln_misc_log: logfile related functions
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
%%% @doc logfile related functions
%%%

-module(mpln_misc_log).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([prepare_log/1, need_rotate/2]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include("types.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc creates log file, shuts down tty log in case of log file
%% opened successfuly.
%%
-spec prepare_log(string()) -> ok | {error, any()}.

prepare_log(File) ->
    filelib:ensure_dir(File),
    error_logger:logfile(close),
    Fn = get_fname(File),
    case error_logger:logfile({open, Fn}) of
        ok ->
            error_logger:tty(false);
        {error, Reason} ->
            {error, Reason}
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc returns a string with base part and time stamp
%%
get_fname(File) ->
    T = mpln_misc_time:get_time_str2(),
    lists:flatten([File ++ "_" ++ T]).

%%-----------------------------------------------------------------------------
%%
%% @doc checks if there was enough time for log to be rotated
%% @since 2011-09-16 13:39
%%
-spec need_rotate(t_datetime(), minute | hour | day | month) -> boolean().

need_rotate(Last, Type) ->
    Cur = calendar:local_time(),
    need_rotate(Last, Type, Cur).

%%-----------------------------------------------------------------------------
%%
%% @doc checks if there was enough time for log to be rotated
%%
-spec need_rotate(t_datetime(), minute | hour | day | month, t_datetime()) ->
    boolean().

need_rotate({{Y, M, D}, {H, Mn, _}}, 'minute', {{Y2, M2, D2}, {H2, Mn2, _}}) ->
    (Y /= Y2) or (M /= M2) or (D /= D2) or (H /= H2) or (Mn /= Mn2);
need_rotate({{Y, M, D}, {H, _, _}}, 'hour', {{Y2, M2, D2}, {H2, _, _}}) ->
    (Y /= Y2) or (M /= M2) or (D /= D2) or (H /= H2);
need_rotate({{Y, M, D}, _}, 'day', {{Y2, M2, D2}, _}) ->
    (Y /= Y2) or (M /= M2) or (D /= D2);
need_rotate({{Y, M, _}, _}, 'month', {{Y2, M2, _}, _}) ->
    (Y /= Y2) or (M /= M2);
need_rotate(_, _, _) ->
    % last resort must be false, so any inconsistencies do not lead
    % to log rotate
    false.

%%-----------------------------------------------------------------------------
