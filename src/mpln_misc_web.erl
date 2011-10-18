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
%%%

-module(mpln_misc_web).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([flatten/1, flatten/2, flatten_r/1, flatten/3]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc flattens a complex structure to a list of {Key, Value} tuples
%% creating the long keys (kind of "k1.k2.k3") when necessary
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
-record(r, {b=[], m, i=0, v, f=false}).

-spec flatten(list() | tuple()) -> string().

flatten(D) ->
    flatten(D, false).

% flatten and remove 'struct' atoms
flatten_r(D) ->
    flatten(D, true).

flatten(D, Struct) ->
    List = flatten(D, [], #r{}),
    L2 = lists:reverse(List),
    lists:map(fun(X) -> make_tuple(X, Struct) end, L2).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
% noodles
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
% in case of {K, V}
make_idx(#r{b=Base}, 'struct') ->
    % 'struct' - JSON decoders put this
    #r{b=Base++['struct'], f=true}
;
make_idx(#r{b=Base, f=F}, K) ->
    #r{b=Base++[K], f=F}.

% other
make_idx(#r{m=undefined, i=Idx} = R) ->
    R#r{m=Idx, i=0}
;
make_idx(#r{b=Base, m=Mid, i=Idx} = R) ->
    R#r{b=Base ++ [Mid], m=Idx, i=0}.

%%-----------------------------------------------------------------------------
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
remove_struct(Key) ->
    [X || X <- Key, X =/= 'struct'].

%%-----------------------------------------------------------------------------
make_tuple(#r{v=Val} = R, Struct) ->
    % Struct: remove or not 'struct' atoms from a key
    Key = make_key(R, Struct),
    {Key, Val}.

%%-----------------------------------------------------------------------------
