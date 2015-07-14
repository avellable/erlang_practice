% A module to implement the Reverse Polish Notation Calculator.
% usage:
%   rpn_calc:rpn("10 2 +")
-module(rpn_calc).
-compile(export_all).

rpn(L) when is_list(L) ->
  [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
  Res.

rpn("+", [N1,N2|Stack]) -> [N1+N2|Stack];
rpn("-", [N1,N2|Stack]) -> [N2-N1|Stack];
rpn("*", [N1,N2|Stack]) -> [N2*N1|Stack];
rpn("/", [N1,N2|Stack]) -> [N2/N1|Stack];
rpn(X, Stack) -> [read(X)| Stack].

read(N) ->
  case string:to_float(N) of
    {error,no_float} -> list_to_integer(N);
    {F, _} -> F
  end.
