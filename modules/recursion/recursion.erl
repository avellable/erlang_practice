-module(recursion).
-compile(export_all).
%% A module for all recursion related practices

% The following function is just a demo of converting mathematical
% expression in recursive function.
% To calculate factorial in mathematical notations, we write,
%
%      /- 1         if n=0
% n! = 
%      \- n*(n-1)!  if n>0
%
% The above mathematical notation becomes
fact(N) when N == 0 -> 1;
fact(N) when N > 0 -> N*fact(N-1).

% The followign function calculates the length of a list.
len([]) -> 0;
len([_|T]) -> 1 + len(T).

% A function to duplicate value of an integer to N times.
duplicate(0, _) -> [];
duplicate(N, Term) when N > 0 -> [Term| duplicate(N-1, Term)].

% A recursive function to reverse a list.
reverse([]) -> [];
reverse([H,T]) -> reverse(T)++[H].

% The tail recursive version of factorial function.
tail_fact(N) -> tail_fact(N,1).
% This demonstrates the use of acumalator. 
tail_fact(0,Accumalator) -> Accumalator;
tail_fact(N,Accumalator) when N > 0 -> tail_fact(N-1, N*Accumalator).

% The tail recursive version of len.
tail_len(L) -> tail_len(L,0).

tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T,1+Acc).

% The tail recursive version of the duplicate function.
tail_duplicate(N, Term) -> tail_duplicate(N, Term, []).

tail_duplicate(0, _, L) -> L;
tail_duplicate(N, Term, L) when N > 0 -> tail_duplicate(N-1, Term, [Term | L]).

% The tail recursive version of the reverse function.
tail_reverse(L) -> tail_reverse(L, []).

tail_reverse([], R) -> R;
tail_reverse([H|T], R) -> tail_reverse(T, [H|R]).

% A tail recursive function for quick sort.
% For empty list return empty
tail_qsort([]) -> [];
tail_qsort(L=[_|_]) -> tail_qsort(L, []).

% The base case
tail_qsort([], Acc) -> Acc;
% This using first element as pivot element.
% Also a tuple is used to store the smallerList, equalList and largerList.
tail_qsort([Pivot|Rest], Acc) -> tail_partition(Pivot, Rest, {[], [Pivot], []}, Acc).
 
tail_partition(_, [], {Smaller, Equal, Larger}, Acc) -> 
  tail_qsort(Smaller, Equal ++ tail_qsort(Larger, Acc));

tail_partition(Pivot, [H|T], {Smaller, Equal, Larger}, Acc) ->
  if H < Pivot ->
      tail_partition(Pivot, T, {[H|Smaller], Equal, Larger}, Acc);
     H > Pivot ->
      tail_partition(Pivot, T, {Smaller, Equal, [H|Larger]}, Acc);
     H == Pivot ->
      tail_partition(Pivot, T, {Smaller, [H|Equal], Larger}, Acc)
    end.
