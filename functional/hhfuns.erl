-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

map(_, []) -> [];
map(Fun, [H|T]) -> [Fun(H)|map(Fun, T)].

incr(X) -> X+1.
decr(X) -> X-1.

filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).

filter(_Pred, [], Filtered) -> Filtered;
filter(Pred, [H|T], Filtered) ->
  case Pred(H) of
    true  -> filter(Pred, T, [H|Filtered]);
    false -> filter(Pred, T, Filtered)
  end.

fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H, Start), T).

reverse(L) -> fold(fun(X, Acc) -> [X|Acc] end, [], L).

map2(F, L) -> reverse(fold(fun(X, Acc) -> [F(X)|Acc] end, [], L)).

filter2(Pred, L) ->
  F = fun(X, Acc) ->
    case Pred(X) of
      true  -> [X|Acc];
      false -> Acc
    end
  end,
  reverse(fold(F, [], L)).