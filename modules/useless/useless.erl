
% The only attribut required to use a module. 
%
% Syntax:
%   -module(name)
%
% where name is an atom
% Note: This has to be the first line of code of a module.
-module(useless).

-author("Mahesh Baheti").
% The 'export' attribute is used to export(make availabel for use) functions 
% from a module.
%
% Syntax:
%   -export([Function1/Arity, Function2/Arity, ..., FunctionN/Arity]).
% where Function1 -  and so on is function name(also an atom).
%       Arity     - is number of arguments that function takes
-export([add/2, hello/0, greet_and_add_two/1]).

% A function.
% 
% Syntax:
%   Name(Args) -> Body
% where, Name - name of the function also an atom.
%        Args - The arguments used
%        Body one or more statements seperated by comma.
add(A,B) ->
	% Adds A & B and returns result.
	% No 'return' is required. The result of last staments in returned.
	A + B.

hello() ->
	io:format("Hello, world!~n").

greet_and_add_two(X) ->
	hello(),
	add(X,2).
