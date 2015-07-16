% Event
% The implementaion of event client(X,Y,Z) designed in 
% http://learnyousomeerlang.com/designing-a-concurrent-application

-module(event).
-compile(export_all).
-include("reminder.hrl").

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, EventName, Delay) ->
  loop(#event_state{server=Server,
                    name=EventName,
                    to_go=normalize(Delay)}).

loop(S = #event_state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T*1000 ->
    if Next =:= [] ->
         Server ! {done, S#event_state.name};
       Next =/= [] ->
         loop(S#event_state{to_go=Next})
    end
  end.

normalize(N) ->
  Limit = 49*24*60*60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

cancel(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.

time_to_go(TimeOut={{_,_},{_,_}}) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
         calendar:datetime_to_gregorian_seconds(Now),
  Sec = if ToGo > 0 -> ToGo;
           ToGo =< 0 -> 0
        end,
  normalize(Sec).
