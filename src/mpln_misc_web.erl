%%%
%%% mpln_misc_web: web related functions
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
%%% @since 2011-10-17 15:23
%%% @license MIT
%%% @doc functions related to web
%%% @TODO rework Base and Mid to use them as Head|Tail. And reverse in
%%% the end. Or better rewrite it (flatten) completely.
%%%

-module(mpln_misc_web).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([flatten/1, flatten/2, flatten_r/1, flatten/3]).
-export([query_string/1, make_string/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%----------------------------------------------------------------------------
%%% Records
%%%----------------------------------------------------------------------------

-record(r, {b=[], m, i=0, v, f=false}).

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc flattens a complex structure to a list of {Key, Value} tuples
%% creating the long keys (kind of "k1.k2.k3") when necessary.
%% Main purpose of flatten is to create a flat list of {K,V} tuples from
%% a deep list after json decoder. All other variants _CAN_ give the _WRONG_
%% result. You are warned. In fact even with a json decoder it can give
%% the wrong data.
%% Example:
%% C = {a, [
%%   b,
%%   2,
%%   {33, c},
%%   {d, [
%%     {5, e},
%%     {f, 6},
%%     {g, [
%%       h,
%%       [i1, i2, i3],
%%       {j, 10}
%%     ]}
%%   ]}
%% ]}
%% gives the following:
%% "a[0]=b&a[1]=2&a[33]=c&a[d][5]=e&a[d][f]=6&a[d][g][0]=h&a[d][g][1][0]=i1&a[d][g][1][1]=i2&a[d][g][1][2]=i3&a[d][g][j]=10"
%% @since 2011-10-17 15:23
%%
-spec flatten(list() | tuple()) -> [{list(), any()}].

flatten(D) ->
    flatten(D, false).

-spec flatten(list() | tuple(), boolean()) -> [{list(), any()}].

flatten_r(D) ->
    % flatten and remove 'struct' atoms
    flatten(D, true).

flatten(D, Struct) ->
    List = flatten(D, [], #r{}),
    L2 = lists:reverse(List),
    lists:map(fun(X) -> make_tuple(X, Struct) end, L2).

%%-----------------------------------------------------------------------------
%%
%% @doc creates a query string from a list of {k,v} tuples. Keys in tuples
%% can be compound.
%% @since 2011-10-18 16:54
%%
-spec query_string(list()) -> string().

query_string(List) ->
    F = fun({K, V}) ->
        Vstr = make_string(V),
        Res = make_query_key(K) ++ io_lib:format("=~s", [Vstr]),
        lists:flatten(Res)
    end,
    List_str = lists:map(F, List),
    string:join(List_str, "&").

%%-----------------------------------------------------------------------------
%%
%% @doc gets binary or list and makes it a string
%% @since 2011-08-11 12:56
%%
-spec make_string(any()) -> string().

make_string(X) when is_integer(X) ->
    integer_to_list(X);
make_string(B) when is_binary(B) ->
    binary_to_list(B);
make_string(A) when is_atom(A) ->
    atom_to_list(A);
make_string(D) ->
    D.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc The main body of flatten is noodles.
%%
-spec flatten(any(), list(), #r{}) -> list().

flatten([], Acc, _R) ->
    error_logger:info_report({"flatten list end", Acc, _R}),
    Acc;
flatten([H|T], Acc, #r{i=Idx} = Key) ->
    New_key = make_idx(Key),
    error_logger:info_report({"flatten list new_idx", H, T, Acc, Key, New_key}),
    New_acc = flatten(H, Acc, New_key),
    flatten(T, New_acc, Key#r{i=Idx+1})
;
flatten({K, V}, Acc, Key) ->
    Ref = make_ref(),
    New_idx = make_idx(Key, K),
    error_logger:info_report({"flatten, 1, k-v", Ref, K, V, Acc, Key, New_idx}),
    New_acc = flatten(V, Acc, New_idx),
    error_logger:info_report({"flatten, 2, k-v", Ref, K, V, Acc, Key, New_acc}),
    New_acc
;
flatten(X, Acc, R) when is_tuple(X) ->
    error_logger:info_report({"flatten tuple", X, Acc, R}),
    L = tuple_to_list(X),
    flatten(L, Acc, R)
;
flatten(X, Acc, R) -> % when not is_list(X) ->
    Item = R#r{v=X},
    error_logger:info_report({"flatten rest", Item, Acc, R}),
    [Item | Acc]
.

%%-----------------------------------------------------------------------------
%%
%% @doc Pushes keys into Base part. For {Key, Value} case the Key replaces
%% last item of Base part (which is an integer index usually). Middle part
%% isn't used here.
%% Marks the item if it has 'struct' to assist in removing it in the end of
%% flatten.
%%
make_idx(#r{b=Base}, 'struct') ->
    % 'struct' - JSON decoders put this
    #r{b=Base++['struct'], f=true}
;
make_idx(#r{b=Base, f=F}, K) ->
    #r{b=Base++[K], f=F}.

% not {Key, Value} tuple
make_idx(#r{m=undefined, i=Idx} = R) ->
    R#r{m=Idx, i=0}
;
make_idx(#r{b=Base, m=Mid, i=Idx} = R) ->
    R#r{b=Base ++ [Mid], m=Idx, i=0}.

%%-----------------------------------------------------------------------------
%%
%% @doc gets an #r{} record and creates a compound key,
%% removing 'struct' atoms from the key if asked.
%%
make_key(#r{b=Base, m=undefined, f=F}, Struct) ->
    make_key_0(Base, F, Struct);
make_key(#r{b=Base, m=Mid, f=F}, Struct) ->
    make_key_0(Base++[Mid], F, Struct).

%%-----------------------------------------------------------------------------
make_key_0(List, true, true) ->
    remove_struct(List);
make_key_0(List, _, _) ->
    List.

%%-----------------------------------------------------------------------------
%%
%% @doc removes 'struct' atoms from a list
%%
remove_struct(Key) ->
    [X || X <- Key, X =/= 'struct'].

%%-----------------------------------------------------------------------------
%%
%% @doc gets an #r{} record and creates a {k,v} tuple possibly with
%% a compound key, removing 'struct' atoms from the key if asked.
%%
-spec make_tuple(#r{}, boolean()) -> {list(), any()}.

make_tuple(#r{v=Val} = R, Struct) ->
    % Struct: remove or not 'struct' atoms from a key
    Key = make_key(R, Struct),
    {Key, Val}.

%%-----------------------------------------------------------------------------
%%
%% @doc creates a [compound] key for a query
%%
make_query_key([]) ->
    ""
;
make_query_key([H]) when is_integer(H) ->
    Str = make_string(H),
    io_lib:format(":~s", [Str])
;
make_query_key([H]) ->
    Str = make_string(H),
    io_lib:format("~s", [Str])
;
make_query_key([H|T]) when is_integer(H) ->
    F = fun(X) ->
        Xstr = make_string(X),
        io_lib:format("[~s]", [Xstr])
    end,
    Str = make_string(H),
    io_lib:format(":~s", [Str]) ++ lists:map(F, T)
;
make_query_key([H|T]) ->
    F = fun(X) ->
        Xstr = make_string(X),
        io_lib:format("[~s]", [Xstr])
    end,
    Str = make_string(H),
    io_lib:format("~s", [Str]) ++ lists:map(F, T)
;
make_query_key(X) ->
    % not a list
    X
.

%%-----------------------------------------------------------------------------
