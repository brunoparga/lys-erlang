-module(event).
-export([start/2, start_link/2, cancel/1, init/3]).
-record(state, {server,
                name="",
                to_go=[0]}).

start(EventName, DateTime) ->
    spawn(?MODULE, init, [self(), EventName, DateTime]).

start_link(EventName, DateTime) ->
    spawn_link(?MODULE, init, [self(), EventName, DateTime]).

cancel(Event) ->
    %% Monitor in case the process is already dead
    Ref = monitor(process, Event),
    Event ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Event, _Reason} ->
            ok
    end.

init(Server, EventName, DateTime) ->
    loop(#state{server=Server,
                name=EventName,
                to_go=time_to_go(DateTime)}).

loop(State = #state{server=Server, to_go=[T|Next]}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after T * 1000 ->
        if Next =:= [] ->
            Server ! {done, State#state.name};
           Next =/= [] ->
            loop(State#state{to_go=Next})
        end
    end.

normalize(Seconds) ->
    Limit = 49*24*60*60,
    [Seconds rem Limit | lists:duplicate(Seconds div Limit, Limit)].

time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
           calendar:datetime_to_gregorian_seconds(Now),
    Secs = if ToGo > 0 -> ToGo;
              ToGo =< 0 ->0
           end,
    normalize(Secs).
