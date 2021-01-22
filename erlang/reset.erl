-module(reset).
-export([ format_process/0, format_process_loop/0, inc_loop2/1, inc_process/0
    ]).

format_process() ->
    spawn(fun format_process_loop/0).

format_process_loop() ->
    receive 
        Message -> io:format(Message),
                   format_process_loop()
    end.


inc_loop2(N) ->
    receive
        reset -> io:format("resetting~n"),
                 inc_loop2(0);
        Inc -> io:format("incrementing ~w by ~w~n", [N, Inc]),
               inc_loop2(N + Inc)
    end.

inc_process() ->
    % spawn(fun () -> inc_loop(0) end).
    % spawn(intro_mike, inc_loop, [0]).
    spawn(?MODULE, inc_loop2, [0]).

% Calc
-record(reset, {}).
-record(inc, {inc, { increment :: number() }}).
-record(mult, {inc, { factor :: number() }}).

