-module(calc_server).

-behaviour(gen_server).

% Schnittstelle für Callbacks
% Hier eine Implementierung des Interface gen_server

-record(reset, {}).
-record(inc, {increment :: number()}).
-record(mult, {factor :: number()}).
-record(divide, {divisor :: number()}).
-record(get, {pid :: pid()}).

-type message() :: #reset{} | #inc{} 
                 | #mult{} | #divide{} | #get{}.

-spec update_calc_state(number(), message()) -> number().
update_calc_state(_N, #reset{}) -> 0;
update_calc_state(N, #inc{increment = Increment}) ->
    N + Increment;
update_calc_state(N, #mult{factor = Factor}) ->
    N * Factor;
update_calc_state(N, #divide{divisor = Divisor}) ->
    N / Divisor;
% muß auch noch Nachricht zurückschicken:
% #get{} ist anders als die anderen
update_calc_state(N, #get{}) -> N.

init