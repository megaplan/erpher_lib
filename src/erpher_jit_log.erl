%%%
%%% erpher_jit_log: functions for just in time logs
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
%%% @since 2012-03-20 13:14
%%% @license MIT
%%% @doc functions for just in time logs
%%%

-module(erpher_jit_log).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([
         prepare_jit_tab/1,
         add_jit_msg/5,
         send_jit_log/2
        ]).

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc create ets table for jit log messages
%% @since 2012-03-20 13:26
%%
prepare_jit_tab(Tag) ->
    ets:new(Tag, [ordered_set, protected, {keypos, 1}]).

%%-----------------------------------------------------------------------------
%%
%% @doc insert jit log message with current timestamp into given ets
%% @since 2012-03-20 13:15
%%
add_jit_msg(Tab, Id1, Id2, Limit, Data) ->
    Now = now(),
    Time = mpln_misc_time:get_gmt_time(Now),
    Ts = {Time, Now},
    Info = {Limit, Id1, Id2, Data},
    ets:insert(Tab, {Ts, Info}).

%%-----------------------------------------------------------------------------
%%
%% @doc send all messages with high enough jit log level from given ets
%% to erpher_rt_stat
%% @since 2012-03-20 13:20
%%
send_jit_log(Conf_level, Tid) ->
    F = fun(X, _) ->
                 send_jit_item(X, Conf_level),
                 none
         end,
    %% simple version of ets:first + ets:next
    ets:foldl(F, none, Tid).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc send one message to erpher_rt_stat if message log level higher than
%% limit
%%
send_jit_item({{Time, Now}, {_Limit, Id1, Id2, Msg}}, Level)
  when Level == max ->
    Bin1 = mpln_misc_web:make_term_binary(Id1),
    Bin2 = mpln_misc_web:make_term_binary(Id2),
    erpher_rt_stat:add(Bin1, Bin2, Time, Now, Msg);

send_jit_item({{Time, Now}, {Limit, Id1, Id2, Msg}}, Level)
  when Level >= Limit ->
    Bin1 = mpln_misc_web:make_term_binary(Id1),
    Bin2 = mpln_misc_web:make_term_binary(Id2),
    erpher_rt_stat:add(Bin1, Bin2, Time, Now, Msg);

send_jit_item(_, _) ->
    ok.

%%-----------------------------------------------------------------------------
