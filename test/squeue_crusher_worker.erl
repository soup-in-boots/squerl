-module(squeue_crusher_worker).
-export([start_link/4]).
-export([init/1]).
-define(GET_VALUE, binary_to_list(crypto:rand_bytes(32))).
-define(FLUSH_COUNT, 1000).

start_link(Sup, Utils, ManageTid, WorkTid) ->
    Ret = proc_lib:start_link(?MODULE, init, [[Sup, Utils, ManageTid, WorkTid]]),
    Ret.

init([Sup, Utils, ManageTid, WorkTid]) ->
    proc_lib:init_ack(Sup, {ok, self()}),
    ec_neural_utils:do_incr(ManageTid, workers, 1),
    main_loop(Utils, ManageTid, WorkTid, 0).

main_loop(Utils, ManageTid, WorkTid, ?FLUSH_COUNT) ->
    ec_neural_utils:do_incr(ManageTid, operations, ?FLUSH_COUNT),
    main_loop(Utils, ManageTid, WorkTid, 0);
main_loop(Utils, ManageTid, WorkTid, Count) ->
    case crypto:rand_uniform(1, 1001) of
        N when N =< 400 ->
            Utils:push(WorkTid, ?GET_VALUE);
        _ ->
            Utils:pop(WorkTid)
    end,
    erlang:garbage_collect(),
    main_loop(Utils, ManageTid, WorkTid, Count + 1).
