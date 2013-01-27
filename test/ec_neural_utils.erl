-module(ec_neural_utils).
-export([do_incr/3, do_incr/4, do_update/4, delete/2, tab2list/1]).
-define(MAX_DEPTH, 1000).

tab2list(Tid) ->
    neural:dump(Tid).

do_incr(Tid, Key, Incr) ->
    do_incr(Tid, Key, Incr, {Key, Incr}).

do_incr(Tid, Key, Incr, Default) ->
    do_incr(Tid, Key, Incr, Default, 0).

do_incr(Tid, Key, Incr, Default, Depth) ->
    case catch neural:increment(Tid, Key, Incr) of
        N when is_integer(N) -> N;
        L when is_list(L) -> L;
        _ ->
            do_make_new(Tid, Key, Incr, Default, Depth, fun do_incr/5, fun do_incr_new_return/2)
    end.

do_update(Tid, Key, UpdateOp, Default) ->
    do_update(Tid, Key, UpdateOp, Default, 0).

do_update(Tid, Key, UpdateOp, Default, Depth) ->
    case catch neural:swap(Tid, Key, UpdateOp) of
        {'EXIT', {badarg, _Stack}} ->
            do_make_new(Tid, Key, UpdateOp, Default, Depth, fun do_update/5, fun do_update_new_return/2);
        _ ->
            true
    end.

do_make_new(Tid, Key, UpdateOps, Default, ?MAX_DEPTH, Retry, Return) ->
    io:format("Aborting bad update operation. Retried ~p times. Tid: ~p; Key: ~p; Retry: ~p; Return: ~p~n", [?MAX_DEPTH, Tid, Key, Retry, Return]),
    error(badarg);
do_make_new(Tid, Key, UpdateOps, Default, Depth, Retry, Return) ->
    case neural:insert_new(Tid, Default) of
        false ->
            Retry(Tid, Key, UpdateOps, Default, Depth + 1);
        true ->
            Return(UpdateOps, Default)
    end.

do_incr_new_return(L = [{_,_}|_], Default) ->
    [ element(N, Default) || {N, _} <- L ];
do_incr_new_return({N, _}, Default) ->
    element(N, Default);
do_incr_new_return(N, Default) when is_integer(N) ->
    element(2, Default).

do_update_new_return(_Return, _Default) ->
    true.

delete(Tid, Key) ->
    neural:delete(Tid, Key).
