-module(queue_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([push/2, pop/1]).

push(Pid, Value) ->
    gen_server:cast(Pid, {push, Value}).

pop(Pid) ->
    gen_server:call(Pid, pop).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, queue:new()}.

handle_call(pop, _From, Queue) ->
    {Value, NewQueue} = queue:out(Queue),
    {reply, Value, NewQueue}.

handle_cast({push, Value}, Queue) ->
    {noreply, queue:in(Value, Queue)}.

handle_info(_, Queue) ->
    {noreply, Queue}.

code_change(_OldVsn, Queue, _Extra) ->
    {ok, Queue}.

terminate(_Reason, _Queue) ->
    ok.
