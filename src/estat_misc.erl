%%%
%%% estat_misc: internal statistic handling
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
%%% @since 2012-01-23 18:14
%%% @license MIT
%%% @doc functions that create output of statistic data
%%%

-module(estat_misc).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([make_stat_cur_info/2]).
-export([make_stat_t_info/2]).

%%%----------------------------------------------------------------------------
%%% Defines
%%%----------------------------------------------------------------------------

-define(TABC, "border=1 cellspacing=4 cellpadding=4 frame=border rules=all").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc returns time statistic
%% @since 2012-01-23 18:16
%%
-spec make_stat_t_info(dict(), raw | text | html) -> string() | list().

make_stat_t_info(Dat, raw) ->
    get_stat_t_info(Dat);

make_stat_t_info(Dat, text) ->
    make_stat_t_info_text(Dat);

make_stat_t_info(Dat, html) ->
    make_stat_t_info_html(Dat).

%%-----------------------------------------------------------------------------
%%
%% @doc returns state of queues: name, length.
%% @since 2012-01-23 18:16
%%
-spec make_stat_cur_info(dict(), dict()) -> string().

make_stat_cur_info(D1, D2) ->
    Winfo = make_stat_work_info(D1),
    Qinfo = make_stat_queue_info(D2),
    List = [{"working", Winfo}, {"queued", Qinfo}],
    F = fun({Tag, L}) ->
                    [io_lib:format("~p~n~n", [Tag]),
                     L,
                     io_lib:format("----------------------------------------"
                                   "~n~n", [])
                    ]
            end,
    lists:flatten(lists:map(F, List)).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc creates time statistic as html
%%
make_stat_t_info_html(St) ->
    List = get_stat_t_info(St),
    F = fun({{Dt, Group}, {W_cur, W_max, Q_cur, Q_max}}) ->
                [
                 "<tr>",
                 "<td>", mpln_misc_time:make_str2_int(Dt), "</td>",
                 "<td>", mpln_misc_web:make_string(Group), "</td>",
                 "<td>", mpln_misc_web:make_string(W_cur), "</td>",
                 "<td>", mpln_misc_web:make_string(W_max), "</td>",
                 "<td>", mpln_misc_web:make_string(Q_cur), "</td>",
                 "<td>", mpln_misc_web:make_string(Q_max), "</td>",
                 "</tr>\n"
                ];
           ({K, V}) ->
                [
                 "<tr>",
                 "<td colspan=2>",
                 mpln_misc_web:make_term_string(K),
                 "</td>",
                 "<td colspan=4>",
                 mpln_misc_web:make_term_string(V),
                 "</td>",
                 "</tr>\n"
                ]
        end,
    F_big = fun({Tag, L}) ->
                    [
                     "<html><body>\n<p>\n",
                     "<p>\n",
                     "<table ", ?TABC, ">",
                     "<tr><td colspan=6 bgcolor=\"#CCCCDA\">",
                     mpln_misc_web:make_term_string(Tag),
                     "</td></tr>\n",
                     "<tr>",
                     "<td>time</td>",
                     "<td>group</td>",
                     "<td>work current</td>",
                     "<td>work max</td>\n",
                     "<td>queue current</td>",
                     "<td>queue max</td>",
                     "</tr>\n",
                     lists:map(F, L),
                     "<table>\n",
                     "<p>\n",
                     "</body></html>\n"
                    ]
            end,
    lists:flatten(lists:map(F_big, List)).

%%-----------------------------------------------------------------------------
%%
%% @doc creates time statistic as text
%%
-spec make_stat_t_info_text(dict()) -> string().

make_stat_t_info_text(Dat) ->
    List = get_stat_t_info(Dat),
    F = fun({K, V}) ->
                io_lib:format("~p: ~p~n", [K, V])
        end,
    F_big = fun({Tag, L}) ->
                    [io_lib:format("~p~n~n", [Tag]),
                     lists:map(F, L),
                     io_lib:format("----------------------------------------"
                                   "~n~n", [])
                    ]
            end,
    lists:flatten(lists:map(F_big, List)).

%%-----------------------------------------------------------------------------
%%
%% @doc returns sorted data
%%
-spec get_stat_t_info(dict()) -> [{any(), any()}].

get_stat_t_info(Data) ->
    lists:sort(dict:to_list(Data)).

%%-----------------------------------------------------------------------------
%%
%% @doc fetches names and sizes from a working stat dictionary
%%
-spec get_stat_work_info(dict()) -> [{any(), non_neg_integer()}].

get_stat_work_info(Data) ->
    F = fun(Gid, Cur, Acc) ->
                Len = length(Cur),
                [{Gid, Len} | Acc]
        end,
    dict:fold(F, [], Data).

%%-----------------------------------------------------------------------------
%%
%% @doc creates a {queued_group, length} list
%%
-spec make_stat_queue_info(dict()) -> list().

make_stat_queue_info(_St) ->
    make_list([]).

%%-----------------------------------------------------------------------------
%%
%% @doc creates a {working_group, length} list
%%
-spec make_stat_work_info(dict()) -> list().

make_stat_work_info(Dat) ->
    List = get_stat_work_info(Dat),
    make_list(List).

%%-----------------------------------------------------------------------------
%%
%% @doc makes a text representation of a {key, value} list
%%
-spec make_list(list()) -> list().

make_list(List) ->
    F = fun({K, V}) ->
                io_lib:format("~p: ~p~n", [K, V])
        end,
    lists:map(F, List).

%%-----------------------------------------------------------------------------