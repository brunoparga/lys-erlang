-module(ppool_serv).
-behaviour(gen_server).
-export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-define(SPEC(MFA),
    {
        worker_sup,
        {ppool_worker_sup, start_link, [MFA]},
        temporary,
        10_000,
        supervisor,
        [ppool_worker_sup]
    }
).
-record(state, {limit=0, sup, refs, queue=queue:new()}).

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

run(Name, Args) ->
    gen_server:call(Name, {run, Args}).

sync_queue(Name, Args) ->
    gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
    gen_server:cast(Name, {async, Args}).

stop(Name) ->
    gen_server:call(Name, stop).

init({Limit, MFA, Sup}) ->
    %% We need to find the Pid of the worker supervisor from here,
    %% but alas, this would be calling the supervisor while it
    %% waits for us!
    self() ! {start_worker_supervisor, Sup, MFA},
    {ok, #state{limit=Limit, refs=gb_sets:empty()}}.

handle_info({'DOWN', Ref, process, _Pid, _}, State=#state{refs=Refs}) ->
    case gb_sets:is_element(Ref, Refs) of
        true ->
            handle_down_worker(Ref, State);
        false ->
            {noreply, State}
    end;

handle_info({start_worker_supervisor, Sup, MFA}, State = #state{}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    link(Pid),
    {noreply, State#state{sup=Pid}};
handle_info(Msg, State) ->
    io:format("Unknown message: ~p~n", [Msg]),
    {noreply, State}.

handle_call(
    {run, Args},
    _From,
    State=#state{limit=N, sup=Sup, refs=Refs}
) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = monitor(process, Pid),
    {
        reply,
        {ok, Pid},
        State#state{limit=N-1, refs=gb_sets:add(Ref, Refs)}
    };

handle_call(
    {run, _Args},
    _From,
    State=#state{limit=N}
) when N =< 0 ->
    {reply, noalloc, State};

handle_call(
    {sync, Args},
    _From,
    State=#state{limit=N, sup=Sup, refs=Refs}
) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = monitor(process, Pid),
    {
        reply,
        {ok, Pid},
        State#state{limit=N-1, refs=gb_sets:add(Ref, Refs)}
    };

handle_call({sync, Args}, From, State=#state{queue=Queue}) ->
    {noreply, State#state{queue=queue:in({From, Args}, Queue)}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(
    {async, Args},
    State=#state{limit=N, sup=Sup, refs=Refs}
) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = monitor(process, Pid),
    {noreply, State#state{limit=N-1, refs=gb_sets:add(Ref, Refs)}};

handle_cast({async, Args}, State=#state{queue=Queue}) ->
    {noreply, State#state{queue=queue:in(Args, Queue)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_down_worker(Ref, State=#state{limit=N, sup=Sup, refs=Refs}) ->
    case queue:out(State#state.queue) of
        {{value, {From, Args}}, Queue} ->
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
            gen_server:reply(From, {ok, Pid}),
            {noreply, State#state{refs=NewRefs, queue=Queue}};
        {{value, Args}, Queue} ->
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
            {noreply, State#state{refs=NewRefs, queue=Queue}};
        {empty, _} ->
            {noreply, State#state{limit=N+1, refs=gb_sets:delete(Ref, Refs)}}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
