-module(useless).
-vsn(["0.1.0"]).
-export([add/2, hello/0, greet_and_add_two/1]).

add(A, B) ->
  A + B.

%% Shows greetings.
hello() ->
  io:format("Hello, world!~n").

greet_and_add_two(X) ->
  hello(),
  add(X, 2).
