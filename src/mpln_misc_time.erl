%%%
%%% mpln_misc_time: time processing functions
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
%%% @doc functions related to time processing
%%%

-module(mpln_misc_time).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([get_ts/0]).
-export([uuid/0]).
-export([get_time_str_us/0, get_time_str_us/1]).
-export([get_time_str/0, get_time_str/1]).
-export([get_time_str2/0, get_time_str2/1]).
-export([get_time/0, get_time/1, parse_unix_time/1]).
-export([get_gmt_time/0, get_gmt_time/1]).
-export([parse_to_gregorian_seconds/1]).
-export([gregorian_seconds_str/1]).
-export([make_gregorian_seconds/0, make_gmt_gregorian_seconds/0]).
-export([make_gregorian_seconds/1, make_gmt_gregorian_seconds/1]).
-export([make_short_str/2, make_short_str2/2]).
-export([make_str2_int/1]).

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
%% @doc creates a short string representation of a datetime that has been
%% cut up to the given interval
%% @since 2011-12-22 13:48
%%
-spec make_short_str(t_datetime(), minute | hour | day | month)
    -> string().

make_short_str({{Y, M, D}, {H, Min, _}}, minute) ->
    make_short_str_format("~4.10.0B~2.10.0B~2.10.0B-~2.10.0B~2.10.0B",
        [Y, M, D, H, Min]);

make_short_str({{Y, M, D}, {H, _, _}}, hour) ->
    make_short_str_format("~4.10.0B~2.10.0B~2.10.0B-~2.10.0B", [Y, M, D, H]);

make_short_str({{Y, M, D}, _}, day) ->
    make_short_str_format("~4.10.0B~2.10.0B~2.10.0B", [Y, M, D]);

make_short_str({{Y, M, _}, _}, month) ->
    make_short_str_format("~4.10.0B~2.10.0B~2.10.0B-~2.10.0B", [Y, M]).

%%-----------------------------------------------------------------------------
%%
%% @doc creates a full string representation (ymd-hms) of a datetime that
%% has been cut up to the given interval
%% @since 2011-12-22 13:48
%%
-spec make_short_str2(t_datetime(), minute | hour | day | month)
    -> string().

make_short_str2({D, {H, Min, _}}, minute) ->
    make_str2_int({D, {H, Min, 0}});

make_short_str2({D, {H, _, _}}, hour) ->
    make_str2_int({D, {H, 0, 0}});

make_short_str2({D, _}, day) ->
    make_str2_int({D, {0, 0, 0}});

make_short_str2({{Y, M, _}, _}, month) ->
    make_str2_int({{Y, M, 1}, {0, 0, 0}}).

%%-----------------------------------------------------------------------------
%%
%% @doc converts input string with unix time to integer
%% @since 2011-12-20 19:39
%%
-spec parse_unix_time(string()) -> non_neg_integer().

parse_unix_time(Str) ->
    erlang:list_to_integer(Str).

%%-----------------------------------------------------------------------------
%%
%% @doc converts gregorian seconds to string representation
%% @since 2011-12-21 14:12
%%
-spec gregorian_seconds_str(non_neg_integer()) -> string().

gregorian_seconds_str(S) ->
    D = calendar:gregorian_seconds_to_datetime(S),
    make_str_int(D).

%%-----------------------------------------------------------------------------
%%
%% @doc converts unix time to gregorian seconds
%% @since 2011-12-21 15:45
%%
-spec make_gregorian_seconds(non_neg_integer()) -> non_neg_integer().

make_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds(
        calendar:now_to_local_time(now())).

make_gregorian_seconds(Time) ->
    S = Time rem 1000000,
    M = trunc(Time / 1000000),
    calendar:datetime_to_gregorian_seconds(
        calendar:now_to_local_time({M, S, 0})
        ).

-spec make_gmt_gregorian_seconds(non_neg_integer()) -> non_neg_integer().

make_gmt_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds(
        calendar:now_to_universal_time(now())).

make_gmt_gregorian_seconds(Time) ->
    S = Time rem 1000000,
    M = trunc(Time / 1000000),
    calendar:datetime_to_gregorian_seconds(
        calendar:now_to_universal_time({M, S, 0})
        ).

%%-----------------------------------------------------------------------------
%%
%% @doc converts input string with unix time to integer and corrects it
%% to gregorian seconds
%% @since 2011-12-20 19:47
%%
-spec parse_to_gregorian_seconds(string()) -> non_neg_integer().

parse_to_gregorian_seconds(Str) ->
    N = parse_unix_time(Str),
    make_gregorian_seconds(N).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time in gregorian seconds for current or given time
%% @since 2011-12-20 19:21
%%
-spec get_time() -> non_neg_integer().

get_time() ->
    get_time(now()).


-spec get_time(tuple()) -> non_neg_integer().

get_time(Now) ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time(Now)).

-spec get_gmt_time() -> non_neg_integer().

get_gmt_time() ->
    get_gmt_time(now()).


-spec get_gmt_time(tuple()) -> non_neg_integer().

get_gmt_time(Now) ->
    calendar:datetime_to_gregorian_seconds(
        calendar:now_to_universal_time(Now)).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time string in gregorian seconds for current time
%% @since 2011-07-15
%%
-spec get_ts() -> string().

get_ts() ->
    Str = get_time(),
    integer_to_list(Str).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time string (y-m-d h:m:s.us) for current time
%% @since 2011-07-15
%%
-spec get_time_str_us() -> string().

get_time_str_us() ->
    get_time_str_us(now()).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time string (y-m-d h:m:s.us) for defined time
%% @since 2011-07-15
%%
-spec get_time_str_us(t_now()) -> string().

get_time_str_us({_, _, Us} = Now) ->
    {Date, {H, M, S}} = calendar:now_to_local_time(Now),
    make_str_float({Date, {H, M, S + Us/1000000.0}}).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time string (y-m-d h:m:s) for current time
%% @since 2011-08-16 19:05
%%
-spec get_time_str() -> string().

get_time_str() ->
    get_time_str(now()).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time string (y-m-d h:m:s) for defined time
%% @since 2011-08-16 19:05
%%
-spec get_time_str(t_now()) -> string().

get_time_str(Now) ->
    DT = calendar:now_to_local_time(Now),
    make_str_int(DT).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time string (ymd-hms) for current time
%% @since 2011-09-16 14:33
%%
-spec get_time_str2() -> string().

get_time_str2() ->
    get_time_str2(now()).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time string (ymd-hms) for defined time
%% @since 2011-09-16 14:33
%%
-spec get_time_str2(t_now()) -> string().

get_time_str2(Now) ->
    DT = calendar:now_to_local_time(Now),
    make_str2_int(DT).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time string (ymd-hms) for given time
%% @since 2011-07-15
%%
-spec make_str2_int(t_datetime()) -> string().

make_str2_int(DateTime) ->
    make_str("~4.10.0B~2.10.0B~2.10.0B-~2.10.0B~2.10.0B~2.10.0B", DateTime).

%%-----------------------------------------------------------------------------
%%
%% @doc returns binary filled by current time to be used as uuid
%% @since 2011-07-15
%%
-spec uuid() -> binary().

uuid() ->
    {A, B, C} = now(),
    <<A:32, B:32, C:32>>.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc returns time string according to the given format and datetime
%%
make_short_str_format(Format, List) ->
    Str = io_lib:format(Format, List),
    lists:flatten(Str).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time string (y-m-d h:m:s.us) for given time
%% @since 2011-07-15
%%
-spec make_str_float(t_datetime_f()) -> string().

make_str_float(DateTime) ->
    make_str("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~9.6.0f", DateTime).

%%
%% @doc returns time string (y-m-d h:m:s) for given time
%% @since 2011-07-15
%%
-spec make_str_int(t_datetime()) -> string().

make_str_int(DateTime) ->
    make_str("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", DateTime).

%%
%% @doc returns time string (y-m-d h:m:s) according to the given format and
%% datetime
%% @since 2011-07-15
%%
-spec make_str(string(), t_datetime() | t_datetime_f()) -> string().

make_str(Format, {{Year, Mon, Day}, {Hour, Min, Sec}}) ->
    Str = io_lib:format(Format, [Year, Mon, Day, Hour, Min, Sec]),
    lists:flatten(Str).

%%%----------------------------------------------------------------------------
%%% EUnit tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).
make_str_test() ->
    "0111-01-07 12:05:*********" = make_str(
        "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~9.6.0f",
        {{111, 1, 7}, {12, 5, 503.0045}}),
    "0111-01-07 12:05:03.004500" = make_str(
        "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~9.6.0f",
        {{111, 1, 7}, {12, 5, 3.0045}}).

make_str_float_test() ->
    "0111-01-07 12:05:03.004500" =
        make_str_float({{111, 1, 7}, {12, 5, 3.0045}}),
    "8111-01-07 12:05:*********" =
        make_str_float({{8111, 1, 7}, {12, 5, 503.0045}}).

get_time_str_us_test() ->
    ?_assertException(error, function_clause,
        get_time_str_us({{8111, 1, 7}, {12, 5, 503.0045}})
        )
.
-endif.
