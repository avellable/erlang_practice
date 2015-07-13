-module(hof).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X,Y) -> X() + Y().

% A function to increment each element in a list.
inc([]) -> [];
inc([H|T]) -> [H+1|inc(T)].

% A function to decrement each element in a list. 
dec([]) -> [];
dec([H|T]) -> [H-1|dec(T)].

% To make a higher order function to make the common part
% of above two functions in one function.
map([], _) -> [];
map([H|T], F) -> [F(H)| map(T,F)].

% Now the increment & decrement functions become.
dec_min(X) -> X-1.

inc_min(X) -> X+1.

% Now the call can be made to both of these functions in 
% following manner.
% map([1,2,3,4], fun dec_min/1)

% Usinf anonymous functions the same call can be made as
% map([1,2,3,4], fun(X) -> X +1 end)
