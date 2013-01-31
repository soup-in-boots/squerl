-module(queue_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([push/2, pop/1, block_pop/1, block_pop/2]).

-record(state, {
        terms,
        term_len,
        blocked,
        blocked_len
    }).

push(Pid, Value) ->
    gen_server:cast(Pid, {push, Value}).

pop(Pid) ->
    gen_server:call(Pid, pop).

block_pop(Pid) ->
    block_pop(Pid, 5000).

block_pop(Pid, Timeout) ->
    gen_server:call(Pid, block_pop, Timeout).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{
            terms       = queue:new(),
            term_len    = 0,
            blocked     = queue:new(),
            blocked_len = 0
        }}.

handle_call(pop, _From, State = #state{term_len = 0}) ->
    {reply, empty, State};
handle_call(pop, _From, State = #state{terms = Terms, term_len = TermLen}) ->
    {{value, V}, NewTerms} = queue:out(Terms),
    {reply, V, State#state{terms = NewTerms, term_len = TermLen - 1}};
handle_call(block_pop, From, State = #state{term_len = 0, blocked = Blocked, blocked_len = BlockedLen}) ->
    {noreply, State#state{blocked = queue:in(From, Blocked), blocked_len = BlockedLen + 1}};
handle_call(block_pop, _From, State = #state{terms = Terms, term_len = TermLen}) ->
    {{value, V}, NewTerms} = queue:out(Terms),
    {reply, V, State#state{terms = NewTerms, term_len = TermLen - 1}}.

handle_cast({push, Value}, State = #state{terms = Terms, term_len = TermLen, blocked_len = 0}) ->
    {noreply, State#state{terms = queue:in(Value, Terms), term_len = TermLen + 1}};
handle_cast({push, Value}, State = #state{blocked = Blocked, blocked_len = BlockedLen}) ->
    {{value, B}, NewBlocked} = queue:out(Blocked),
    gen_server:reply(B, {'$queue_server_bpop_response', Value}),
    {noreply, State#state{blocked = NewBlocked, blocked_len = BlockedLen - 1}}.

handle_info(_, Queue) ->
    {noreply, Queue}.

code_change(_OldVsn, Queue, _Extra) ->
    {ok, Queue}.

terminate(_Reason, _Queue) ->
    ok.
