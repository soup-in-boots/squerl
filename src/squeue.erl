-module(squeue).

-export([new/1, push/2, pop/1, block_pop/1, block_pop/2]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

new(_Name) ->
    ?nif_stub.

push(_Name, _Value) ->
    ?nif_stub.

pop(_Name) ->
    ?nif_stub.

block_pop(Name) ->
    block_pop(Name, 5000).

block_pop(Name, Timeout) ->
    block_pop(Name, self(), Timeout).

block_pop(Name, Self, Timeout) ->
    case do_bpop(Name, Self) of
        '$squeue_bpop_wait' ->
            wait_for_bpop(Timeout);
        {'$squeue_bpop_repsonse', V} ->
            V
    end.

wait_for_bpop(Timeout) ->
    receive
        {'$squeue_bpop_response', V} -> V
    after
        Timeout ->
            error(timeout)
    end.

do_bpop(_Name, _Self) ->
    ?nif_stub.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = new(),
    ?assertEqual(ok, myfunction(Ref)).

-endif.
