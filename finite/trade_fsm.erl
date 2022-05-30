-module(trade_fsm).
-behaviour(gen_fsm).

%% Public API
-export([
    start/1,
    start_link/1,
    trade/2,
    accept_trade/1,
    make_offer/2,
    retract_offer/2,
    ready/1,
    cancel/1
]).

%% gen_fsm callbacks
-export([
    init/1,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
]).

%% custom state names
-export([
    idle/2,
    idle/3,
    idle_wait/2,
    idle_wait/3,
    negotiate/2,
    negotiate/3,
    wait/2,
    ready/2,
    ready/3
]).

-record(state, {
    name = "",
    other,
    ownitems=[],
    otheritems=[],
    monitor,
    from
}).

%%% PUBLIC API
start(Name) -> gen_fsm:start(?MODULE, [Name], []).

start_link(Name) -> gen_fsm:start_link(?MODULE, [Name], []).

%% ask for a begin session. Returns when/if the other accepts
trade(OwnPid, OtherPid) -> gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30_000).

%% Accept someone's trade offer
accept_trade(OwnPid) -> gen_fsm:sync_send_event(OwnPid, accept_negotiate).

%% Send an item on the table to be traded
make_offer(OwnPid, Item) -> gen_fsm:send_event(OwnPid, {make_offer, Item}).

%% Cancel trade offer
retract_offer(OwnPid, Item) -> gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%% Mention that you're ready for a trade. When the other
%% player also declares being ready, the trade is done
ready(OwnPid) -> gen_fsm:sync_send_event(OwnPid, ready, infinity).

%% Cancel the transaction
cancel(OwnPid) -> gen_fsm:sync_send_all_state_event(OwnPid, cancel).

%%% gen_fsm callbacks
init(Name) -> {ok, idle, #state{name=Name}}.

%% the other player has sent this cancel event
%% stop whatever we're doing and shut down!
handle_event(cancel, _StateName, State=#state{}) ->
    notice(State, "received cancel event", []),
    {stop, other_cancelled, State};
handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

%% this cancel event comes from the client. We must warn the other
%% player that we have a quitter!
handle_sync_event(cancel, _From, _StateName, State=#state{}) ->
    notify_cancel(State#state.other),
    notice(State, "cancelling trade, sending cancel event", []),
    {stop, cancelled, ok, State};
%% Note: DO NOT reply to unexpected calls. Let the caller crash!
handle_sync_event(Event, _From, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

%% The other FSM went down
handle_info({'DOWN', Ref, process, Pid, Reason}, _, State=#state{other=Pid, monitor=Ref}) ->
    notice(State, "other side dead", []),
    {stop, {other_down, Reason}, State};
handle_info(Info, StateName, Data) ->
    unexpected(Info, StateName),
    {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%% Transaction completed
terminate(normal, ready, State=#state{}) ->
    notice(State, "FSM exiting", []);
terminate(_Reason, _StateName, _StateData) ->
    ok.

%%% FSM to FSM functions
%% Ask the other FSM's Pid for a trade session
ask_negotiate(OtherPid, OwnPid) -> gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

%% Forward the client message accepting the transaction
accept_negotiate(OtherPid, OwnPid) -> gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

%% forward a client's offer
do_offer(OtherPid, Item) -> gen_fsm:send_event(OtherPid, {do_offer, Item}).

%% forward a client's offer cancellation
undo_offer(OtherPid, Item) -> gen_fsm:send_event(OtherPid, {undo_offer, Item}).

%% Ask the other side if he's ready to trade
are_you_ready(OtherPid) -> gen_fsm:send_event(OtherPid, are_you_ready).

%% Reply that the side is not ready to trade
%% i.e is not in 'wait' state
not_yet(OtherPid) -> gen_fsm:send_event(OtherPid, not_yet).

%% Tells the other FSM that the user is currently waiting
%% for the ready state. State should transition to 'ready'
am_ready(OtherPid) -> gen_fsm:send_event(OtherPid, 'ready!').

%% Acknowledge that the FSM is in a ready state
ack_trans(OtherPid) -> gen_fsm:send_event(OtherPid, ack).

%% ask if ready to commit
ask_commit(OtherPid) -> gen_fsm:sync_send_event(OtherPid, ask_commit).

%% begin the synchronous commit
do_commit(OtherPid) -> gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->gen_fsm:send_all_state_event(OtherPid, cancel).

%%% States
idle({ask_negotiate, OtherPid}, State=#state{}) ->
    Ref = monitor(process, OtherPid),
    notice(State, "~p asked for a trade negotiation", [OtherPid]),
    {next_state, idle_wait, State#state{other=OtherPid, monitor=Ref}};
idle(Event, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

idle({negotiate, OtherPid}, From, State=#state{}) ->
    ask_negotiate(OtherPid, self()),
    notice(State, "asking user ~p for a trade", [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state, idle_wait, State#state{other=OtherPid, monitor=Ref, from=From}};
idle(Event, _From, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

idle_wait({ask_negotiate, OtherPid}, State=#state{other=OtherPid}) ->
    gen_fsm:reply(State#state.from, ok),
    notice(State, "starting negotiation", []),
    {next_state, negotiate, State};
% The other side has accepted our offer. Move to negotiate state
idle_wait({accept_negotiate, OtherPid}, State=#state{other=OtherPid}) ->
    gen_fsm:reply(State#state.from, ok),
    notice(State, "starting negotiation", []),
    {next_state, negotiate, State};
idle_wait(Event, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

idle_wait(accept_negotiate, _From, State=#state{other=OtherPid}) ->
    accept_negotiate(OtherPid, self()),
    notice(State, "accepting negotiation", []),
    {reply, ok, negotiate, State};
idle_wait(Event, _From, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

negotiate({make_offer, Item}, State=#state{ownitems=OwnItems}) ->
    do_offer(State#state.other, Item),
    notice(State, "offering ~p", [Item]),
    {next_state, negotiate, State#state{ownitems=add(Item, OwnItems)}};
%% Own side retracting an item offer
negotiate({retract_offer, Item}, State=#state{ownitems=OwnItems}) ->
    undo_offer(State#state.other, Item),
    notice(State, "cancelling offer on ~p", [Item]),
    {next_state, negotiate, State#state{ownitems=remove(Item, OwnItems)}};
%% Other side offering an item
negotiate({do_offer, Item}, State=#state{otheritems=OtherItems}) ->
    notice(State, "other player offering ~p", [Item]),
    {next_state, negotiate, State#state{otheritems=add(Item, OtherItems)}};
%% Other side retracting an item offer
negotiate({undo_offer, Item}, State=#state{otheritems=OtherItems}) ->
    notice(State, "other player cancelling offer on ~p", [Item]),
    {next_state, negotiate, State#state{otheritems=remove(Item, OtherItems)}};
negotiate(are_you_ready, State=#state{other=OtherPid}) ->
    io:format("Other user ready to trade.~n"),
    notice(
        State,
        "Other user ready to transfer goods:~n"
        "You get ~p,~nthe other side gets ~p.~n",
        [State#state.otheritems, State#state.ownitems]
    ),
    not_yet(OtherPid),
    {next_state, negotiate, State};
negotiate(Event, Data) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, Data}.

negotiate(ready, From, State=#state{other=OtherPid}) ->
    are_you_ready(OtherPid),
    notice(State, "asking if ready, waiting", []),
    {next_state, wait, State#state{from=From}};
negotiate(Event, _From, State) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, State}.

wait({do_offer, Item}, State=#state{otheritems=OtherItems}) ->
    gen_fsm:reply(State#state.from, offer_changed),
    notice(State, "other side offering ~p", [Item]),
    {next_state, negotiate, State#state{otheritems=add(Item, OtherItems)}};
wait({undo_offer, Item}, State=#state{otheritems=OtherItems}) ->
    gen_fsm:reply(State#state.from, offer_changed),
    notice(State, "other side canecelling offer of ~p", [Item]),
    {next_state, negotiate, State#state{otheritems=remove(Item, OtherItems)}};
wait(are_you_ready, State=#state{}) ->
    am_ready(State#state.other),
    notice(State, "asked if ready, and I am. Waiting for the same reply", []),
    {next_state, wait, State};
wait(not_yet, State=#state{}) ->
    notice(State, "other not ready yet", []),
    {next_state, wait, State};
wait('ready!', State=#state{}) ->
    am_ready(State#state.other),
    ack_trans(State#state.other),
    gen_fsm:reply(State#state.from, ok),
    notice(State, "other side is ready. Moving to ready state", []),
    {next_state, ready, State};
%% Don't care about these!
wait(Event, Data) ->
    unexpected(Event, wait),
    {next_state, wait, Data}.

ready(ack, State=#state{}) ->
    case priority(self(), State#state.other) of
        true ->
            try
                notice(State, "asking for commit", []),
                ready_commit = ask_commit(State#state.other),
                notice(State, "ordering commit", []),
                ok = do_commit(State#state.other),
                notice(State, "committing...", []),
                commit(State),
                {stop, normal, State}
            catch Class:Reason ->
                %% abort! Either ready_commit or do_commit failed
                notice(State, "commit failed", []),
                {stop, {Class, Reason}, State}
            end;
        false ->
            {next_state, ready, State}
    end;
ready(Event, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.

ready(ask_commit, _From, State) ->
    notice(State, "replying to ask_commit", []),
    {reply, ready_commit, ready, State};
ready(do_commit, _From, State) ->
    notice(State, "committing...", []),
    commit(State),
    {stop, normal, ok, State};
ready(Event, _From, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.

%%% Utility functions
%% Send players a notice. This could be messages to their clients
%% but for our purposes, outputting to the shell is enough.
notice(#state{name=Name}, Str, Args) -> io:format("~s: " ++ Str ++ "~n", [Name|Args]).

%% Unexpected allows to log unexpected messages
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

%% add an item to an item list
add(Item, Items) -> [Item|Items].

%% remove an item from an item list
remove(Item, Items) -> Items -- [Item].

priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.

commit(State = #state{}) ->
    io:format(
        "Transaction completed for ~s. "
        "Items sent are: ~n~p,~nreceived are:~n~p.~n"
        "This operation should have some atomic save "
        "in a database.~n",
        [State#state.name, State#state.ownitems, State#state.otheritems]
    ).
