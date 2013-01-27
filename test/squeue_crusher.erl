-module(squeue_crusher).
-export([test_squeue/0, test_server/0]).

test_squeue() ->
    neural:new(squeue_manage, []),
    squeue:new(squeue_work),
    test(squeue, squeue_manage, squeue_work).

test_server() ->
    neural:new(squeue_manage, []),
    {ok, Pid} = queue_server:start_link(),
    test(queue_server, squeue_manage, Pid).

test(Mode, ManageTid, WorkTid) ->
    squeue_crusher_sup:start_link(Mode, ManageTid, WorkTid),
    squeue_crusher_worker_sup:start_child(Mode, ManageTid, WorkTid).
    
