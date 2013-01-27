-module(squeue_crusher_manager).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-record(state, {
        utils,
        manage,
        worker,
        timer,
        last_update
    }).

start_link(Type, ManageTid, WorkTid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Type, ManageTid, WorkTid], []).

init([Utils, ManageTid, WorkTid]) ->
    TRef = timer:send_interval(5000, update),
    State = #state{
        utils       = Utils,
        manage      = ManageTid,
        worker      = WorkTid,
        timer       = TRef,
        last_update = os:timestamp()
    },
    {ok, State}.

handle_call(_Call, _From, State) ->
    {reply, undefined, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(update, State = #state{utils = Utils, manage = ManageTid, worker = WorkTid, last_update = LastTime}) ->
    %% Calculate time offset
    CurrTime = os:timestamp(),
    Delta = timer:now_diff(CurrTime, LastTime) / 1000000,

    %% Gather statistics
    Workers     = ec_neural_utils:do_incr(ManageTid, workers, 0),
    Operations  = ec_neural_utils:do_incr(ManageTid, operations, 0),
    PerSecond   = Operations / Delta,
    PerWorker   = PerSecond / Workers,

    %% Report.
    io:format("Workers: ~p; Operations: ~p; Rate: ~p; Rate per Worker: ~p~n", [Workers, Operations, PerSecond, PerWorker]),
    ec_neural_utils:do_incr(ManageTid, operations, -Operations),
    squeue_crusher_worker_sup:start_child(Utils, ManageTid, WorkTid),
    erlang:garbage_collect(),
    {noreply, State#state{last_update = CurrTime}};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
