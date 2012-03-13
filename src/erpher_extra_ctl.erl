%%%
%%% erpher_extra_ctl: functions for external requests
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
%%% @since 2012-02-22 16:31
%%% @license MIT
%%% @doc functions for external requests, e.g. stats, memory, etc.
%%%

-module(erpher_extra_ctl).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([
         start/0
        ]).

%%%----------------------------------------------------------------------------
%%% Defines
%%%----------------------------------------------------------------------------

-define(RPC_TIMEOUT, 15000).
-define(EXTRA_SEP, "\n===\n").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc 
%% @since 2012-02-22 19:48
%%
-spec start() -> none().

start() ->
    Node = get_dest_node(),
    Commands = get_rest_args(),
    Res = [{X, catch cmd(X, Node)} || X <- Commands],
    Text = make_output(Res),
    io:format("~s", [Text]),
    halt(0).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc extract destination node name from arguments
%%
get_dest_node() ->
    get_arg_atom(nodename).

%%-----------------------------------------------------------------------------
%%
%% @doc extract defined parameter from arguments
%%
get_arg_atom(X) ->
    case init:get_argument(X) of
        {ok, [[Str|_]|_]} ->
            list_to_atom(Str);
        _ ->
            error_logger:error_report({"parameter undefined", X}),
            halt(1)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc extract all arguments after '-extra' and convert them to atoms
%%
get_rest_args() ->
    L1 = init:get_plain_arguments(),
    [list_to_atom(X) || X <- L1].

%%-----------------------------------------------------------------------------
%%
%% @doc convert argument and response to string
%%
-spec make_output([{atom(), any()}]) -> string().

make_output(List) ->
    Res = [[?EXTRA_SEP, make_one_output(X)] || X <- List],
    lists:flatten(Res).

make_one_output({Cmd, {'EXIT', Reason}}) ->
    io_lib:format("error, EXIT: ~p~n~p~n", [Cmd, Reason]);

make_one_output({Cmd, {error, Reason}}) ->
    io_lib:format("error, error: ~p~n~p~n", [Cmd, Reason]);

make_one_output({Cmd, {badrpc, Reason}}) ->
    io_lib:format("error, badrpc: ~p~n~p~n", [Cmd, Reason]);

make_one_output({Cmd, Res}) ->
    io_lib:format("ok: ~p~n~p~n", [Cmd, Res]).

%%-----------------------------------------------------------------------------
%%
%% @doc call remote node for given {M,F,A} with timeout
%%
call(Node, {M, F, A}) ->
    rpc:call(Node, M, F, A, ?RPC_TIMEOUT).

%%-----------------------------------------------------------------------------
cmd(ejobman_stop, Node) ->
    call(Node, {application, stop, [ejobman]});

cmd(ejobman_start, Node) ->
    call(Node, {application, start, [ejobman]});

cmd(eworkman_stop, Node) ->
    call(Node, {application, stop, [eworkman]});

cmd(eworkman_start, Node) ->
    call(Node, {application, start, [eworkman]});

cmd(ecomet_stop, Node) ->
    call(Node, {application, stop, [ecomet]});

cmd(ecomet_start, Node) ->
    call(Node, {application, start, [ecomet]});

cmd(sum_pids_memory, Node) ->
    {Mem, _Nproc} = cmd(get_procs_info, Node),
    Mem;

cmd(nprocs, Node) ->
    {_Mem, Nproc} = cmd(get_procs_info, Node),
    Nproc;

cmd(get_procs_info, Node) ->
    call(Node, {estat_misc, get_procs_info, []});

cmd(ecomet_nprocs, Node) ->
    call(Node, {ecomet_server, get_stat_procs, []});

cmd(ecomet_nprocs_mem, Node) ->
    call(Node, {ecomet_server, get_stat_procs_mem, []});

cmd(config_reload, Node) ->
    call(Node, {eworkman_handler, config_reload, []});

cmd(_, _Node) ->
    {error, unknown_request}.

%%-----------------------------------------------------------------------------
