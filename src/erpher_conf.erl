%%%
%%% erpher_conf: functions to handle config
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
%%% @since 2012-03-11 18:16
%%% @license MIT
%%% @doc functions for handling app config
%%%

-module(erpher_conf).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([
         add_config/1
        ]).

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc add config to app env taking into account config deep structure
%% @since 2012-03-11 18:17
%%
-spec add_config([{atom() | binary(), any()}]) -> ok.

add_config(Config) ->
    add_config2(Config).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc iterate over every app from config, merge this app into env
%%
add_config2([]) ->
    ok;

add_config2([{App, Pars} | T]) ->
    Env = application:get_all_env(App),
    Changed = merge_one_app(Env, Pars),
    store_one_app({App, Changed}),
    add_config2(T).

%%-----------------------------------------------------------------------------
%%
%% @doc merge parameters for one app
%%
merge_one_app(Env, Conf) ->
    merge_one_app2(Env, Conf, [], []).

merge_one_app2(Env_rest, [], Env_acc, _Conf_rest) ->
    Env_rest ++ Env_acc;

merge_one_app2(Env, [{Key, Val} = H | T], Env_acc, Conf_rest) ->
    {Env_pars, Env_rest} = get_par_list(Key, Env),
    case get_type(Val) of
        list_of_tuples ->
            % item: {k, v},
            % go deeper
            New_val = merge_one_app2(Env_pars, Val, [], []),
            merge_one_app2(Env_rest, T, [{Key, New_val} | Env_acc], [H|Conf_rest]);
        list_of_lists ->
            % item: [{k,v}...],
            % add list
            New_val = Env_pars ++ Val,
            merge_one_app2(Env_rest, T, [{Key, New_val} | Env_acc], [H|Conf_rest]);
        string ->
            % item: string,
            % replace string
            merge_one_app2(Env_rest, T, [{Key, Val} | Env_acc], [H|Conf_rest]);
        terminal ->
            % item: atom, integer, binary, etc.
            % replace it
            merge_one_app2(Env_rest, T, [{Key, Val} | Env_acc], [H|Conf_rest])
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc return value extracted from Data even if the data is not a {k,v} list.
%% If there is anything besides list in Data it could cause problems
%% (inconsistence in fact. And it depends on your attitude).
%% It can happen when Data contains {atom(), integer()} and config contains
%% for example {atom(), [tuple()]}
%%
get_par_list(Conf_key, Env_data) ->
    case catch proplists:split(Env_data, [Conf_key]) of
        {'EXIT', _} ->
            {[], []};
        {[List|_], Rest} ->
            Val = proplists:get_value(Conf_key, List),
            {Val, Rest}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc identify type of input list: string, list of tuples, list of lists
%%
get_type([H | _T]) when is_integer(H) ->
    % list of integers == string
    string;

get_type([H | _T]) when is_list(H) ->
    list_of_lists;

get_type([{_K, _V}| _T]) ->
    list_of_tuples;

get_type([]) ->
    list_of_tuples;

get_type(_) ->
    terminal.

%%-----------------------------------------------------------------------------
%%
%% @doc store updated app into env
%%
store_one_app({App, Params}) ->
    [application:set_env(App, Par, Val) || {Par, Val} <- Params].

%%-----------------------------------------------------------------------------
