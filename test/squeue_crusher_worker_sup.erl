-module(squeue_crusher_worker_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/3]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Utils, ManageTid, WorkTid) ->
    supervisor:start_child(?MODULE, [Utils, ManageTid, WorkTid]).

init([]) ->
    {ok, {
            {simple_one_for_one, 5, 10},
            [
                {
                    squeue_crusher_worker,
                    {squeue_crusher_worker, start_link, [self()]},
                    transient,
                    5000,
                    worker,
                    [squeue_crusher_worker]
                }
            ]
        }}.
