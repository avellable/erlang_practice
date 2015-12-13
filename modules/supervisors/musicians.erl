-module(musicians).
-behaviour(gen_server).

-export([stop/1, start_link/2]).
%% export behviour exports
-export([handle_call/3, handle_cast/2, init/1, handle_info/2, code_change/3, terminate/2]).

-record(state, {name="", role, skill=good}).
-define(DELAY, 750).

start_link(Role, Skill) ->
  gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) ->
  gen_server:stop(Role,stop).


%% Behaviour exports
init([Role, Skill]) ->
  process_flag(trap_exit, true),
  random:seed(now()),
  TimeToPlay = random:uniform(3000),
  Name = pick_name(),
  StrRole = atom_to_list(Role),
  io:format("~s started playing ~s!~n", [Name, StrRole]),
  {ok, #state{name=Name, role=Role, skill=Skill}, TimeToPlay}.

handle_call(stop, _From, S = #state{}) ->
  {stop, normal, ok, S};
handle_call(_Message, _From, S) ->
  {noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
  {noreply, S, ?DELAY}.

handle_info(timeout, S=#state{name=N, skill=good}) ->
  io:format("~s produced sound.~n", [N]),
  {noreply, S, ?DELAY};
handle_info(timeout, S=#state{name=N, skill=bad}) ->
  case random:uniform(5) of
    1 ->
      io:format("~s played a bad note!~n", [N]),
      {stop, bad_note, S};
    _ ->
      io:format("~s produced sound.~n", [N]),
      {noreply, S, ?DELAY}
  end;
handle_info(_Message, S) ->
  {noreply, S, ?DELAY}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, S) ->
  io:format("~s left playing ~s!~n", [S#state.name, S#state.role]);
terminate(bad_note, S) ->
  io:format("~s was kicked out!~n", [S#state.name]);
terminate(shutdown, S) ->
  io:format("The band was shutdown. ~s is jobless~n", [S#state.name]);
terminate(_Reason, S) ->
  io:format("~s kicked out of unknown reasons.~n", [S#state.name]).

%% Private functions
pick_name() ->
  lists:nth(random:uniform(10), names()).

names() ->
  ["Joe", "John", "Jim", "Tim", "Monu", "Sonu", "Chintu", "Pintu", "Lalu", "Golu"].
