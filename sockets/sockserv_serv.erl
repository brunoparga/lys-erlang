-module(sockserv_serv).
-behaviour(gen_server).

-record(state, {name,       % player's name
                next,       % next step, used when initializing
                socket}).   % the current socket

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(TIME, 800).
-define(EXP, 50).

start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
  %% properly seeding the process
  <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
  random:seed({A, B, C}),
  %% Because accepting a connection is a blocking function call,
  %% we cannot do it in here. Forward to the server loop!
  gen_server:cast(self(), accept),
  {ok, #state{socket=Socket}}.

%% We never need you, handle_call!
handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(accept, State = #state{socket = ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  %% Keep the number of children constant
  sockserv_sup:start_socket(),
  send(AcceptSocket, "What's your character's name?", []),
  {noreply, State#state{socket=AcceptSocket, next=name}};

handle_cast(roll_stats, State = #state{socket = Socket}) ->
  Roll = pq_stats:initial_roll(),
  send(Socket,
    "Stats for your character:~n"
    "  Charisma: ~B~n"
    "  Constitution: ~B~n"
    "  Dexterity: ~B~n"
    "  Intelligence: ~B~n"
    "  Strength: ~B~n"
    "  Wisdom: ~B~n~n"
    "Do you agree to these? y/n~n",
    [Points || {_Name, Points} <- lists:sort(Roll)]),
  {noreply, State#state{next = {stats, Roll}}};

%% The player has accepted the stats! Start the game!
handle_cast(stats_accepted, State = #state{name = Name, next = {stats, Stats}}) ->
  processquest:start_player(Name, [{stats, Stats}, {time, ?TIME}, {lvlexp, ?EXP}]),
  processquest:subscribe(Name, sockserv_pq_events, self()),
  {noreply, State#state{next=playing}};

%% Events coming in from process quest
%% We know this because all these events' tuples start with the
%% name of the player as part of the internal protocol defined for us
handle_cast(Event, State = #state{name = Name, socket = Socket})
  when element(1, Event) =:= Name ->
  [handle_game_event(EventStr, Socket) || EventStr <- sockserv_trans:to_str(Event)],
  {noreply, State}.

handle_info({tcp, _Socket, "quit"++_}, State) ->
  processquest:stop_player(State#state.name),
  gen_tcp:close(State#state.socket),
  {stop, normal, State};

handle_info({tcp, _Socket, Str}, State = #state{next=name}) ->
  Name = line(Str),
  gen_server:cast(self(), roll_stats),
  {noreply, State#state{name = Name, next = stats}};

handle_info({tcp, Socket, Str}, State = #state{socket = Socket, next = {stats, _}}) ->
  handle_accept_stats(line(Str), Socket),
  {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};

handle_info({tcp_error, _Socket}, State) ->
  {stop, normal, State};

handle_info(Event, State) ->
  io:format("Unexpected: ~p~n", [Event]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) -> ok;
terminate(Reason, _State) -> io:format("terminate reason: ~p~n", [Reason]).

%% Private functions
send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.

%% Let's get rid of the white space and ignore whatever's after.
%% makes it simpler to deal with telnet.
line(Str) ->
  hd(string:tokens(Str, "\r\n ")).

handle_accept_stats("y", _) -> gen_server:cast(self(), stats_accepted);
handle_accept_stats("n", _) -> gen_server:cast(self(), roll_stats);
handle_accept_stats(_, Socket) -> send(Socket, "Answer with y (yes) or n (no)", []).

handle_game_event({wait, Time}, _) -> timer:sleep(Time);
handle_game_event(IoList, Socket) -> send(Socket, IoList, []).
