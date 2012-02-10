%%%
%%% mpln_node_ctl: node control for windows
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
%%% @since 2011-09-23 19:37
%%% @license MIT
%%% @doc small node control tool for running nodes. Similar to nodetool.
%%% Needed on windows only to stop the running node.
%%% Config reload function can be used everywhere.
%%%

-module(mpln_node_ctl).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([stop/0, restart/0, reboot/0, test/0, config_reload/0]).

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc commands to send to target node
%%
test() ->
    cmd(test).

stop() ->
    cmd(stop).

restart() ->
    cmd(restart).

reboot() ->
    cmd(reboot).

config_reload() ->
    eworkman_handler:config_reload().

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc establishes a connection to the target node, otherwise just halts
%%
connect(Type, Node_str, Cookie) ->
    Ctl_node = list_to_atom("ctl_" ++ Node_str),
    case start_net(Type, Ctl_node) of
        {ok, _Pid} ->
            ok;
        {error, Reason} ->
            io:format("Can't start node ~p: ~p\n", [Ctl_node, Reason]),
            halt(1)
    end,
    erlang:set_cookie(Ctl_node, list_to_atom(Cookie)),
    Node = list_to_atom(Node_str),
    case {net_kernel:hidden_connect_node(Node), net_adm:ping(Node)} of
        {true, pong} ->
            ok;
        {_, pang} ->
            io:format("Node ~p not responding to pings.\n", [Node]),
            halt(1)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc calls net_kernel:start with appropriate node and type
%%
start_net("-sname", Node) ->
    net_kernel:start([Node, shortnames]);
start_net("-name", Node) ->
    net_kernel:start([Node, longnames]).

%%-----------------------------------------------------------------------------
%%
%% @doc sends a command to the target node and halts own VM
%%
cmd(Cmd) ->
    Data = get_node_name(),
    cmd2(Cmd, Data).

cmd2(_, {error, Reason}) ->
    io:format("some error: ~p~n", [Reason]),
    halt(1);
cmd2(Cmd, {ok, Type, Node_str, Cookie}) ->
    connect(Type, Node_str, Cookie),
    Node = list_to_atom(Node_str),
    case Cmd of
        'stop' ->
            io:format("stop: ~p\n", [rpc:call(Node, init, stop, [], 60000)]);
        'restart' ->
            io:format("restart: ~p\n", [rpc:call(Node, init, restart, [], 60000)]);
        'reboot' ->
            io:format("reboot: ~p\n", [rpc:call(Node, init, reboot, [], 60000)]);
        _ ->
            io:format("unknown command~n", [])        
    end,
    net_kernel:stop(),
    halt(0).

%%-----------------------------------------------------------------------------
%%
%% @doc reads etc/vm.args and extracts nodename and cookie
%%
get_node_name() ->
    case file:get_cwd() of
        {ok, Dir} ->
            get_pars(Dir);
        Err ->
            Err
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc reads vm.args and extracts nodename and cookie
%%
get_pars(Dir) ->
    Fullname = Dir ++ "/etc/vm.args",
    case file:read_file(Fullname) of
        {ok, Bin} ->
            extract_pars(Bin);
        Err ->
            {error, {"read file error", Fullname, Err}}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc extracts nodename and cookie from the binary data
%%
extract_pars(Bin) ->
    case extract_name(Bin) of
        {ok, Par, Node} ->
            case extract_cookie(Bin) of
                {ok, Cookie} ->
                    {ok, Par, Node, Cookie};
                Err2 ->
                    Err2
            end;
        Err ->
            Err
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc extracts nodename from the binary data
%%
extract_name(Bin) ->
    case re:run(Bin, "^(-s?name)\\s+(.*)$",
                [multiline, {capture, all_but_first, list}]) of
        {match, [Par, Val]} ->
            {ok, Par, Val};
        _ ->
            {error, name_not_found}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc extracts cookie from the binary data
%%
extract_cookie(Bin) ->
    case re:run(Bin, "^-setcookie\\s+(.*)$",
                [multiline, {capture, all_but_first, list}]) of
        {match, [Val]} ->
            {ok, Val};
        _ ->
            {error, cookie_not_found}
    end.

%%-----------------------------------------------------------------------------

