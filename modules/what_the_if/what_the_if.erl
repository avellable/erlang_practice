-module(what_the_if).
-compile(export_all).

% 'if' in erlang is just another pattern of guards. Instead of writing in head
% of the function we write it in the body.
% This function tries to demonstrate the concept of 'if' guards in erlang. The
% first three statements succeed when it matches the condition and for other
% cases it doesn't care and assumes always true. 'true' is just like 'else', 
% but not exactly.
% Just like normal guards, the 'if' guard has to always succeed to work. i.e.
% instead of 
% 	'true'
% using 
% 	Movie =/= 7 -> "Who cares?!"
% won't work and we'll get an exception.
which_actor(Movie) ->
	Actor = if Movie == inception  -> "Leonardo di Caprio";
						 Movie == taken -> "Liam Neeson";
						 Movie == scarface -> "Al Pacino";
						 true -> "Who cares?!"
					end,
	{Movie, "has " ++ Actor ++ "!"}.
