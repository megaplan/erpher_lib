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
-export([make_interval_stat_text/1, make_interval_stat/2]).
-export([add_timed_stat/4, set_max_timed_stat/5]).
-export([make_joined_list/1]).
-export([clean_timed_stat/2]).
-export([
         fetch_sum_pids_memory/1
        ]).

%%%----------------------------------------------------------------------------
%%% Defines
%%%----------------------------------------------------------------------------

-define(TABC, "border=1 cellspacing=4 cellpadding=4 frame=border rules=all").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc iterate over a list of pids and sum up the memory consumption for
%% the processes
%% @since 2012-02-22 15:21
%%
-spec fetch_sum_pids_memory(list(pid())) -> non_neg_integer().

fetch_sum_pids_memory(List) ->
    F = fun(Pid, Acc) ->
                case process_info(Pid, memory) of
                    {memory, N} when is_integer(N) ->
                        Acc + N;
                    _ ->
                        Acc
                end
        end,
    lists:foldl(F, 0, List).

%%-----------------------------------------------------------------------------
%%
%% @doc cleans stat storage (ets) from unnecessary items
%% @since 2012-02-03 15:26
%%
-spec clean_timed_stat(atom(), non_neg_integer()) -> ok.

clean_timed_stat(Tab, Limit) ->
    Keys = find_time_part_keys(Tab), % sorted list
    Len = length(Keys),
        if Len > Limit ->
                Delta = Len - Limit,
                {Del, _} = lists:split(Delta, Keys),
                del_timed_items(Tab, Del);
           true ->
                ok
        end.

%%-----------------------------------------------------------------------------
%%
%% @doc joins several lists with same time and different tags into one
%% list with common time
%% @since 2012-02-02 16:32
%%
-spec make_joined_list(list()) -> list().

make_joined_list(List) ->
    D = dict:new(),
    F = fun ({{Time, {Tag, 'work'}}, V1, V2}, Acc) ->
                Key = {Time, Tag},
                case dict:find(Key, Acc) of
                    error ->
                        dict:store(Key, {V1, V2, -1, -1}, Acc);
                    {ok, {_Pw1, _Pw2, Pq1, Pq2}} ->
                        dict:store(Key, {V1, V2, Pq1, Pq2}, Acc)
                end;
            ({{Time, {Tag, 'queued'}}, V1, V2}, Acc) ->
                Key = {Time, Tag},
                case dict:find(Key, Acc) of
                    error ->
                        dict:store(Key, {-1, -1, V1, V2}, Acc);
                    {ok, {Pw1, Pw2, _Pq1, _Pq2}} ->
                        dict:store(Key, {Pw1, Pw2, V1, V2}, Acc)
                end;
            (_, Acc) ->
                Acc
    end,
    D2 = lists:foldl(F, D, List),
    lists:sort(dict:to_list(D2))
.

%%-----------------------------------------------------------------------------
%%
%% @doc increments counter for time (shortened to time step) and tag
%% in ets table. Stored data: {key, cur, max}
%% @since 2012-02-02 13:32
%%
-spec set_max_timed_stat(atom(), 'hour' | 'minute', tuple(), any(),
                         non_neg_integer()) -> true.

set_max_timed_stat(Table, Step, Time, Tag, Val) ->
    Tm = mpln_misc_time:short_time(Time, Step),
    Key = {Tm, Tag},
    case ets:lookup(Table, Key) of
        [] ->
            ets:insert(Table, {Key, Val, Val});
        [{_Key, _Prev_n, Prev_max}] ->
            Max = erlang:max(Prev_max, Val),
            ets:insert(Table, {Key, Val, Max})
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc increments counter for time (shortened to time step) and tag
%% in ets table
%% @since 2012-02-02 13:32
%%
-spec add_timed_stat(atom(), 'hour' | 'minute', tuple(), any()) -> true.

add_timed_stat(Table, Step, Time, Tag) ->
    Tm = mpln_misc_time:short_time(Time, Step),
    Key = {Tm, Tag},
    case ets:lookup(Table, Key) of
        [] ->
            ets:insert(Table, {Key, 1});
        [{_, N}] ->
            ets:insert(Table, {Key, N+1})
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc gets input list of {tag, {dict, dict, dict}} tuples. Transforms
%% every dict to a list of {new_tag, data} tuples, where new_tag can be
%% time + new_tag or just any new_tag
%% @since 2012-01-25 14:40
%%
-spec make_interval_stat_text([{atom(), {dict(), dict(), dict()}}]) -> string().

make_interval_stat_text(List) ->
    make_interval_stat(text, List).

-spec make_interval_stat(text | html, [{atom(), {dict(), dict(), dict()}}]) ->
                                string().

make_interval_stat(Type, List) ->
    F = fun(X, Acc) ->
                Res = make_stat_one_source(Type, X),
                [Res | Acc]
        end,
    Res = lists:foldl(F, [], List),
    lists:flatten(Res).

%%-----------------------------------------------------------------------------
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
%%
%% @doc gets a tag and a dict and creates a list of text {key, val} items
%% from the dict
%%
-spec make_one_interval_stat(text | html, atom(), dict()) -> list().

make_one_interval_stat(text, Tag, Data) ->
    List = get_stat_t_info(Data),
    Text = make_list(List),
    Tag_str = mpln_misc_web:make_term_string(Tag),
    [Tag_str, "\n", Text, "\n\n"].

%%-----------------------------------------------------------------------------
%%
%% @doc converts input tag to string
%%
make_one_interval_tag(text, Tag) ->
    mpln_misc_web:make_term_string(Tag).

%%
%% @doc returns text separator as a string
%%
make_one_interval_sep(text) ->
    "\n".

%%-----------------------------------------------------------------------------
%%
%% @doc gets type, tag and dicts of stat data and creates text containing
%% several items of pretty printed data
%%
make_stat_one_source(Type, {Tag, {Day, Hour, Min}}) ->
    F = fun({Cur_tag, Cur_dat}, Acc) ->
                Res = make_one_interval_stat(Type, Cur_tag, Cur_dat),
                [Res | Acc]
        end,
    Res = lists:foldl(F, [],
                      [
                       {day, Day},
                       {hour, Hour},
                       {min, Min}
                      ]),
    Tag_str = make_one_interval_tag(Type, Tag),
    Sep_str = make_one_interval_sep(Type),
    [Tag_str, Sep_str, Res, Sep_str].

%%-----------------------------------------------------------------------------
%%
%% @doc removes items with given time from a storage
%%
-spec del_timed_items(atom(), list()) -> ok.

del_timed_items(Tab, List) ->
    L2 = [{X, true} || X <- List],
    Dict = dict:from_list(L2),
    Key_del = find_time_full_keys(Tab, Dict),
    [ets:delete(Tab, X) || X <- Key_del],
    ok.

-spec find_time_full_keys(atom(), dict()) -> list().

%%
%% @doc extracts full keys for given times from a storage
%%
find_time_full_keys(Tab, Dict) ->
    % ets:insert(Table, {Key, Val, Val});
    % ets:insert(Table, {Key, 1});
    F = fun({{Time, _Tag} = Key, _}, Acc) ->
                upd_full_keys_data(Dict, Acc, Time, Key);
           ({{Time, _Tag} = Key, _, _}, Acc) ->
                upd_full_keys_data(Dict, Acc, Time, Key);
           (_, Acc) ->
                Acc
        end,
    Key_data = ets:foldl(F, dict:new(), Tab),
    dict:fetch_keys(Key_data).

%%
%% @doc stores input key into dictionary to ensure uniqueness
%%
-spec upd_full_keys_data(dict(), dict(), tuple(), tuple()) -> dict().

upd_full_keys_data(Dict, Acc, Time, Key) ->
    case dict:find(Time, Dict) of
        {ok, _} ->
            dict:store(Key, true, Acc);
        _ ->
            Acc
    end.

%%
%% @doc extracts time part of a keys from a storage
%%
-spec find_time_part_keys(atom()) -> list().

find_time_part_keys(Tab) ->
    % ets:insert(Table, {Key, Val, Val});
    % ets:insert(Table, {Key, 1});
    F = fun({{Time, _Tag}, _}, Acc) ->
                dict:store(Time, true, Acc);
           ({{Time, _Tag}, _, _}, Acc) ->
                dict:store(Time, true, Acc);
           (_, Acc) ->
                Acc
        end,
    Dat = ets:foldl(F, dict:new(), Tab),
    lists:sort(dict:fetch_keys(Dat)).

%%-----------------------------------------------------------------------------
