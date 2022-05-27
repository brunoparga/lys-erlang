-module(evserv).
-export([
    start/0,
    start_link/0,
    terminate/0,
    subscribe/1,
    add_event/3,
    init/0,
    cancel/1,
    listen/1
]).

-record(state, {events, clients}).
-record(event, {name="", description="", pid, timeout={{1970,1,1}, {0,0,0}}}).

start() ->
    register(?MODULE, Pid=spawn(?MODULE, init, [])),
    Pid.

start_link() ->
    register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
    Pid.

terminate() ->
    ?MODULE ! shutdown.

subscribe(Pid) ->
    Ref = monitor(process, whereis(?MODULE)),
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    receive
        {Ref, ok} ->
            {ok, Ref};
        {'DOWN', Ref, process, _Pid, Reason} ->
            {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

cancel(Name) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {cancel, Name}},
    receive
        {Ref, ok} -> ok
    after 5000 ->
        {error, timeout}
    end.

listen(Delay) ->
    receive
        Msg = {done, _Name, _Description} ->
            [Msg | listen(0)]
    after Delay*1000 ->
        []
    end.

init() ->
    %% Loading events from a static file could be done here.
    %% You would need to pass an argument to init telling where the
    %% resource to find the events is. Then load it from here.
    %% Another option is to just pass the events straight to the server
    %% through this function.
    loop(#state{events=orddict:new(), clients=orddict:new()}).

loop(State = #state{}) ->
    receive
        {Pid, MsgRef, {subscribe, Client}} ->
            Ref = monitor(process, Client),
            NewClients = orddict:store(Ref, Client, State#state.clients),
            Pid ! {MsgRef, ok},
            loop(State#state{clients=NewClients});
        {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
            case valid_datetime(TimeOut) of
                true ->
                    EventPid = event:start_link(Name, TimeOut),
                    NewEvents = orddict:store(
                        Name,
                        #event{
                            name=Name,
                            description=Description,
                            pid=EventPid,
                            timeout=TimeOut
                        },
                        State#state.events
                    ),
                    Pid ! {MsgRef, ok},
                    loop(State#state{events=NewEvents});
                false ->
                    Pid ! {MsgRef, {error, bad_timeout}},
                    loop(State)
            end;
        {Pid, MsgRef, {cancel, Name}} ->
            Events = case orddict:find(Name, State#state.events) of
                        {ok, Event} ->
                            event:cancel(Event#event.pid),
                            orddict:erase(Name, State#state.events);
                        error ->
                            State#state.events
                    end,
            Pid ! {MsgRef, ok},
            loop(State#state{events=Events});
        {done, Name} ->
            case orddict:find(Name, State#state.events) of
                {ok, Event} ->
                    send_to_clients(
                        {done, Event#event.name, Event#event.description}, State#state.clients
                    ),
                    NewEvents = orddict:erase(Name, State#state.events),
                    loop(State#state{events=NewEvents});
                error ->
                    %% This may happen if we cancel an event and
                    %% it fires at the same time
                    loop(State)
            end;
        shutdown ->
            %% Could save the state to disk here
            exit(shutdown);
        {'DOWN', Ref, process, _Pid, _Reason} ->
            loop(State#state{clients=orddict:erase(Ref, State#state.clients)});
        code_change ->
            ?MODULE:loop(State);
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(State)
    end.

valid_datetime({Date, Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause ->  %% not in {{Y, M, D}, {H, Min, S}} format
            false
    end;
valid_datetime(_) ->
    false.

valid_time({H, Min, S}) -> valid_time(H, Min, S).
valid_time(H, Min, S) when H >= 0, H < 24,
                           Min >=0, Min < 60,
                           S >= 0, S < 60 -> true;
valid_time(_, _, _) -> false.

send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).
