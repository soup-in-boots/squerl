-module(squeue_crusher_sup).
-behaviour(supervisor).
-export([start_link/3, init/1]).

start_link(Utils, ManageTid, WorkTid) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Utils, ManageTid, WorkTid]).

init([Utils, ManageTid, WorkTid]) ->
    {ok, {
            {one_for_one, 5, 10},
            [
                {
                    squeue_crusher_worker_sup,
                    {squeue_crusher_worker_sup, start_link, []},
                    permanent,
                    5000,
                    supervisor,
                    [squeue_crusher_worker_sup]
                },
                {
                    squeue_crusher_manager,
                    {squeue_crusher_manager, start_link, [Utils, ManageTid, WorkTid]},
                    permanent,
                    5000,
                    worker,
                    [squeue_crusher_manager]
                }
            ]
        }}.
