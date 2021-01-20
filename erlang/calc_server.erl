-module(calc_server).

-behaviour(gen_server).
% gen_server ist Teil des OTP-Frameworks, das bei Erlang dabei ist
% Open Telephone Platform
% Hier eine Implementierung des Interface gen_server
% Schnittstelle für Callbacks
-export([init/1, handle_cast/2, handle_call/3,
         start/0]).

start() ->
    gen_server:start(?MODULE, 0, []).
                           %  ^^^^^^^^^^^^ wird zum  Argument von init


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

init(InitialN) -> {ok, InitialN}.

% Module:handle_cast(Request, State) -> Result
% Types
% Request = term()
% State = term()
% Result = {noreply,NewState} | 

handle_cast(Message, N) -> {noreply, update_calc_state(N, Message)}.

% Ein Request, der keine Antwort erfordert: cast (asynchron)
% Ein Request, der eine Antwort erfordert: call (synchron)

% Module:handle_call(Request, From, State) -> Result
	
% Types
% Request = term()
% From = {pid(),Tag}
% State = term()
% Result = {reply,Reply,NewState}

handle_call(#get{}, _From, N) -> {reply, N, N + 1}.

