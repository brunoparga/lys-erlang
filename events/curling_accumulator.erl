-module(curling_accumulator).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {teams=orddict:new(), round=0}).

init([]) -> {ok, #state{}}.

handle_event({set_teams, TeamA, TeamB}, State=#state{teams=Teams}) ->
    NewTeams = orddict:store(TeamA, 0, orddict:store(TeamB, 0, Teams)),
    {ok, State#state{teams=NewTeams}};
handle_event({add_points, Team, N}, State=#state{teams=Teams}) ->
    NewTeams = orddict:update_counter(Team, N, Teams),
    {ok, State#state{teams=NewTeams}};
handle_event(next_round, State=#state{}) ->
    {ok, State#state{round = State#state.round + 1}};
handle_event(_Event, Pid) ->
    {ok, Pid}.

handle_call(game_data, State=#state{teams=Teams, round=Round}) ->
    {ok, {orddict:to_list(Teams), {round, Round}}, State};
handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
