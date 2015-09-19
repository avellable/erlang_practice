-module(bascun).
-compile(export_all).

%===============================================================================
% Following three function illustrate the three basic concepts of concurrency
% in erlang.
% * Spawing
% * Receiving Messages
% * Sending Messages

toothless1() ->
  receive
    fly ->
      io:format("How about no?!");
    fish ->
      io:format("Thank you!");
    _->
      io:format("Snot, Snot!")
  end.

toothless2() ->
  receive
    {From, fly} ->
      From ! "How about no?!";
    {From, fish} ->
      From ! "Thank you!";
    {From, _} ->
      From ! "Snot, Snot!"
  end.

toothless3() ->
  receive
    {From, fly} ->
      From ! "How about no?!",
      toothless3();
    {From, fish} ->
      From ! "Thank you!";
    {From, _} ->
      From ! "Snot, Snot!",
      toothless3()
  end.

%===============================================================================

%===============================================================================
% A better explanation of above three concepts in following functions.

% Abstracts spawning of process.
start(Foodlist) ->
  spawn(?MODULE, fridge, [Foodlist]).

% Abstracts the internal protocol to store food.
store(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 ->
    timeout
  end.

% Abstracts the internal protocol to take food.
take(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 ->
    timeout
  end.

% The main function to implement actual storing and taking out.
fridge(Foodlist) ->
  receive
    {From, {store, Food}} ->
      From ! {self(), ok},
      fridge([Food|Foodlist]);

    {From, {take, Food}} ->
      case lists:member(Food, Foodlist) of
        true ->
          From ! {self(), {ok, Food}},
          fridge(lists:delete(Food, Foodlist));
        false ->
          From ! {self(), not_found},
          fridge(Foodlist)
      end;

    terminate ->
      ok
  end.
