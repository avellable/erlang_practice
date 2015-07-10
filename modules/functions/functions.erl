-module(functions).
-compile(export_all). % bad idea

% A function with 'function clause'. 
% Function clauses should be seperated by ';'. The whole
% function is called function declaration. The function 
% declaration ends with a function clause ending with a '.'.
% This function in other langauges will look like,
%
%	function greet(Gender,Name)
%		if Gender == male then
%			print("Hello, Mr. %s!", Name)
%		else if Gender == female then
%			print("Hello, Mrs. %s!", Name)
%		else
%			print("Hello, %s!", Name)
%	end
%
% In erlang using 'pattern matching' which can be done using 
% 'function clauses', it can be written as follows.
greet(male, Name) ->
	io:format("Hello, Mr. ~s", [Name]);
greet(female, Name) ->
	io:format("Hello, Ms. ~s", [Name]);
greet(_, Name) ->
	io:format("Hello, ~s", [Name]).

% A function to return head of a list. Similar to list:hd().
% This function is to demonstrate the pattern of matching 
% [Head | Tail] in erlang.
head([H|_]) -> H.

% A function to return second element of a list.
second([_, S| _]) -> S.

% This function is a beauty. It demonstrates the concept of bound 
% and unbound variables in erlang.
% It basically checks if two elements are same or not?
% How does it works?
% 	- First clause returns true if the args matches.
%			The functions at first will consider the first 'X' as 
%     unbound and assigns it the first value passed to function.
%     For second arg before assigning to X erlang checks if it matches,
%     (because 'X' is already bound now!)if yes, returns true else, 
%			sends to second clause.
%		- Second clause doesn't care about arguments and always returns 
%     false.
same(X,X) -> true;
same(_,_) -> false.

% These functions demonstrates guards in erlang. 
% This one checks the age of person and returns true if allowed to 
% drive.
% The ',' implies 'andalso' if one condition fails the guard fails.
allowed_to_drive(Age) when Age >= 16, Age =< 104 ->
	true;
allowed_to_drive(_) ->
	false.
% The ';' implies 'orelse' if one of the cnditions succeeds, the guard
% succeeds.
not_allowed_to_drive(Age) when Age =< 16 ; Age >= 104 ->
	true;
not_allowed_to_drive(_) ->
	false.
% Note: 'guards' do not allow to use user defined functions.
%
% Note: Note: I've compared ',' and ';' in guards to the operators 'andalso' 
% and 'orelse'. They're not exactly the same, though. The former pair will 
% catch exceptions as they happen while the latter won't. What this means is
% that if there is an error thrown in the first part of the guard X >= N; 
% N >= 0, the second part can still be evaluated and the guard might succeed;
% if an error was thrown in the first part of X >= N orelse N >= 0, the 
% second part will also be skipped and the whole guard will fail.

% Nesting of guards: only 'andalso' and 'orelse' can be nested inside guards. 
% This means (A orelse B) andalso C is a valid guard, while (A; B), C is not. 
% Given their different use, the best strategy is often to mix them as necessary.
