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
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

%%%----------------------------------------------------------------------------
%%% EUnit tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).

get_test_config() ->
    [
    {ejobman, [
        {temp_rt_key_for_group, <<"new">>},
        {log0, "%ROOT%/var/log/erpher/ej"},
        {pid_file0, "/var/run/erpher/erpher.pid"},
        {debug, [
                {msg, 0},
                {json, 0},
                {config, 0},
                {job, 2},
                {run, 2}
        ]},
        {rabbit, [
                {port, 5672},
                {host, '127.0.0.1'},
                {user, <<"guest">>},
                {password, <<"guest">>},
                {vhost , <<"/">>},
                {exchange , <<"ejobman">>},
                {exchange_type0 , <<"topic">>},
                {queue , <<"test_queue_2">>},
                {routing_key , <<"new">>}
        ]},
        {estat, [
            {stat_limit_n, 101}, % amount
            {stat_limit_t, 123}, % seconds
            {stat_limit_cnt_h, 26}, % hours
            {stat_limit_cnt_m, 62}, % minutes
            {storage_base, "%ROOT%/var/lib/erpher/estat/es"},
            {rt_info_file, "%ROOT%/var/lib/erpher/rt_info.dat"},
            {keep_time, 72}, % hours
            {rotate_interval, hour},
            {flush_interval, 60}, % seconds
            {flush_number, 250}, % amount
            {log_procs_interval, 60}, % seconds
            {debug, [
                {stat, 4},
                {file, 2},
                {storage, 2},
                {run, 2}
            ]}
        ]},
        {group_handler, [
            {http_connect_timeout, 15001},
            {http_timeout, 3600001},
            {max_children, 2}, % default group
            {job_groups, [
                [
                    {name, <<"g1">>},
                    {max_children, 3}
                ],
                [
                    {name, <<"g2">>},
                    {max_children, 7}
                ],
                [
                    {name, <<"g3">>},
                    {max_children, 10}
                ]
            ]},
            {schema_rewrite, [
                [
                    {src_host_part, "test.megahost"},
                    % true - on, false - off, other - don't change
                    {https, false}
                ],
                [
                    {src_type, regex},
                    {src_host_part, ".+"},
                    % true - on, false - off, other - don't change
                    {https, true}
                ]
            ]},
            {url_rewrite, [
                [
                    {src_host_part, "host2.localdomain"},
                    {dst_host_part, "127.0.0.1"}
                ],
                [
                    {src_host_part, "mdt.megaplan"},
                    {dst_host_part, "192.168.9.183"}
                ],
                [
                    {src_host_part, "192.168.9.183"},
                    {dst_host_hdr, "promo.megaplan"}
                ],
                [
                    {src_host_part, "promo.megaplan"},
                    {dst_host_part, "192.168.9.183"},
                    {dst_host_hdr, "promo.megaplan"}
                ],
                [
                    {src_type, regex},
                    {src_host_part, "127\\.0\\.0\\.\\d+"},
                    {dst_host_part, "127.0.0.1"},
                    {dst_host_hdr, "host3.localdomain"}
                ],
                [
                    {src_type, regex},
                    {src_host_part, "127\\.\\d+\\.\\d+\\.\\d+"},
                    {dst_host_part, "127.0.0.2"},
                    {dst_host_hdr, "host2.localdomain"}
                ]
            ]},
            {debug, [
                    {handler_child, 2},
                    {handler_run, 2},
                    {handler_job, 2},
                    {msg, 2},
                    {run, 2},
                    {job, 4},
                    {job_result, 1},
                    {job_queue, 2},
                    {http, 3},
                    {config, 0},
                    {rewrite, 0}
            ]}
        ]},
        {handler, [
            {job_log0, "%ROOT%/var/log/erpher/job"},
            % never | minute | hour | day | {dow, 0..7} | month | year
            {job_log_rotate, day},
            {debug,
                [
                    {handler_child, 2},
                    {handler_run, 2},
                    {job, 4},
                    {job_result, 1},
                    {job_queue, 2},
                    {run, 2},
                    {stat, 2},
                    {http, 3},
                    {rewrite, 0}
                ]
            }
        ]}
    ]},
    {eworkman, [
        % which configs to reload and what process (module, in fact) to signal
        {apps, [
            {eworkman, [eworkman_handler]},
            {ejobman, [ejobman_handler, ejobman_receiver, ejobman_stat]},
            {ecomet, [ecomet_server]}
        ]},
        {delay_for_log0, 10}, % ms. Start delay for ejobman to create a log
        % log is common and may also be defined in ejobman
        {log, "%ROOT%/var/log/erpher/ew"},
        % never | minute | hour | day | {dow, 0..7} | month | year
        {log_rotate, day},
        {pid_file, "/var/run/erpher/erpher.pid"},
        {debug, [
                {worker, 3},
                {run, 4},
                {http, 5} % for mochiweb
        ]},
        {web_server_opts, [
            {id, "test_yaws"},
            {docroot, "%ROOT%/var/www/01/www"},
            {sconf, [
                {docroot0, "/"},
                {port, 8143},
                {listen, {0,0,0,0}},
                {ebin_dir, ["%ROOT%/var/www/01/ebin"]},
                %{appmods, [{"/", my_appmod}]},
                %{servername, "host3"},
                {ssl0, [
                    {certfile, "%ROOT%/var/www/01/conf/ssl/localhost-cert.pem"},
                    {keyfile, "%ROOT%/var/www/01/conf/ssl/localhost-key.pem"}
                ]},
                {flags, [
                    {dir_listings, true}
                ]}
            ]},
            {gconf, [
                {yaws_dir, "%ROOT%/lib/yaws-1.91"},
                {logdir, "%ROOT%/var/log/erpher/yaws"},
                {ebin_dir, ["%ROOT%/usr/lib/yaws/custom/ebin"]},
                {include_dir, ["%ROOT%/usr/lib/yaws/custom/include"]},
                {max_connections, nolimit},
                {trace, false},
                {copy_error_log, true},
                {log_wrap_size, 1000000},
                {log_resolve_hostname, false},
                {fail_on_bind_err, true},
                {auth_log, true},
                {id, eworkman_yaws},
                {pick_first_virthost_on_nomatch, true},
                {use_fdsrv, false},
                {subconfigdir, "%ROOT%/var/www/01/conf"}
            ]}
        ]},
        {pools0, [
            [
                {id, p1},
                {min_workers, 1}, % long lasting workers
                {restart_policy, delay},
                {restart_delay, 10}, % sec. Delay before restarting the crashed worker
                {worker_duration, 60}, % seconds. Time before terminate
                {worker, [
                    {name, "/usr/bin/perl -Mstrict -w /etc/erpher/workers/t.pl"},
                    {debug,
                        [
                            {run, 4}
                        ]
                    }]}
            ],
            [
                {id, p2},
                {min_workers, 2}, % long lasting workers
                {worker_duration, 300}, % seconds. Time before terminate
                {worker, [
                    {name, "/etc/erpher/workers/test.sh"},
                    {debug,
                        [
                            {run, 4}
                        ]
                    }]}
            ]
        ]}
    ]}, % eworkman
{test_app, [
]}
    ]
        .

get_par_list_test() ->
    Conf = [
            {
             [
              {key1, "str1"},
              {key2, [{k21, "v21"}, {k22, "v22"}]},
              {key3, v3},
              {key4, 4}
             ],
             key3,
             key31
            }
           ],
    F = fun({List, K1, K2}) ->
                {Res1, _Rest1} = get_par_list(K1, List),
                {Res2, _Rest2} = get_par_list(K2, Res1),
                %?debugFmt("get_par_list_test: ~p, ~p, ~p~n", [K2, Res1, Res2]),
                ?assert(Res2 =:= [])
        end,
    lists:foreach(F, Conf).

merge_one_app1_test() ->
    Env = [
            {key1, "str1"},
            {key2, [{k21, "v21"}, {k22, "v22"}]},
            {key3, v3},
            {key4, 4},
            {key5, 55}
           ],
    Conf = [
            {key1, "str1new"},
            {key2, [{k21, "v21new"}, {k22, "v22new"}]},
            {key3, v32},
            {key4, 42},
            {key6, "v62"}
           ],
    Expected =
        [
            {key1, "str1new"},
            {key2, [{k22, "v22new"}, {k21, "v21new"}]},
            {key3, v32},
            {key4, 42},
            {key5, 55},
            {key6, "v62"}
        ],
    Res = merge_one_app(Env, Conf),
    S = lists:sort(Res),
    %?debugFmt("merge_one_app1_test:~n~p~n", [S]),
    ?assert(Expected =:= S).

merge_one_app2_test() ->
    Env = get_test_config(),
    Conf = [
            {ejobman, [
                       {rabbit, [
                                 {port, 5674},
                                 {host, '127.0.0.2'},
                                 {host2, {127,0,0,3}},
                                 {user, <<"admin">>}
                                ]},
                       {temp_rt_key_for_group, <<"updated">>},
                       {debug, [
                                {json, 11}
                               ]},
                       {estat, [
                                {storage_base, "/tmp/no_file"},
                                {debug, [
                                         {storage, 12}
                                        ]}
                               ]},
                       {group_handler, [
                                        {job_groups, [
                                                      [
                                                       {name, <<"test.notify">>},
                                                       {max_children, 13}
                                                      ]
                                                     ]}
                                        ]}
                       ]}
           ],
    Res = merge_one_app(Env, Conf),
    %?debugFmt("merge_one_app2_test:~n~p~n", [Res]),
    ?assert(is_list(Res)).

-endif.
%%%----------------------------------------------------------------------------
