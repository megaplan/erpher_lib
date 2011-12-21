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
-export([parse_to_gregorian_seconds/1]).

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
gregorian_seconds_str(S) ->
    D = calendar:gregorian_seconds_to_datetime(S),
    make_str_int(D).

%%-----------------------------------------------------------------------------
%%
%% @doc converts input string with unix time to integer and corrects it
%% to gregorian seconds
%% @since 2011-12-20 19:47
%%
parse_to_gregorian_seconds(Str) ->
    N = parse_unix_time(Str),
    T0 = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    T0 + N.

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
%% @doc returns time string (ymd-hms) for given time
%% @since 2011-07-15
%%
-spec make_str2_int(t_datetime()) -> string().

make_str2_int(DateTime) ->
    make_str("~4.10.0B~2.10.0B~2.10.0B-~2.10.0B~2.10.0B~2.10.0B", DateTime).

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
