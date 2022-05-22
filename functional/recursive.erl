-module(recursive).
-export([fac/1, len/1, tail_fac/1, tail_len/1, duplicate/2, reverse/1, sublist/2, zip/2, quick_sort/1]).

fac(0) -> 1;
fac(N) when N > 0  -> N * fac(N-1).

len([]) -> 0;
len([_|T]) -> 1 + len(T).

tail_fac(N) -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N-1, N*Acc).

tail_len(L) -> tail_len(L, 0).

tail_len([], Len) -> Len;
tail_len([_|T], Len) -> tail_len(T, Len+1).

duplicate(N, Term) -> duplicate(N, Term, []).

duplicate(0, _Term, List) -> List;
duplicate(N, Term, List) when N > 0 -> duplicate(N-1, Term, [Term|List]).

reverse(L) -> reverse(L, []).

reverse([], List) -> List;
reverse([H|T], List) -> reverse(T, [H|List]).

sublist(List, Count) -> lists:reverse(sublist(List, Count, [])).

sublist(_List, 0, Sublist) -> Sublist;
sublist([], _Count, Sublist) -> Sublist;
sublist([H|T], Count, Sublist) -> sublist(T, Count - 1, [H|Sublist]).

zip(List1, List2) -> lists:reverse(zip(List1, List2, [])).

zip([], _, Tuples) -> Tuples;
zip(_, [], Tuples) -> Tuples;
zip([H1|T1], [H2|T2], Tuples) -> zip(T1, T2, [{H1, H2}|Tuples]).

quick_sort([]) -> [];
quick_sort([Pivot|Rest]) ->
  {Smaller, Larger} = partition(Pivot, Rest, [], []),
  quick_sort(Smaller) ++ [Pivot] ++ quick_sort(Larger).

partition(_Pivot, [], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
  if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
     H >  Pivot -> partition(Pivot, T, Smaller, [H|Larger])
  end.
