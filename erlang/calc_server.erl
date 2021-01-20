-module(calc_server).

-behaviour(gen_server).
% gen_server ist Teil des OTP-Frameworks, das bei Erlang dabei ist
% Open Telephone Platform
% Hier eine Implementierung des Interface gen_server
% Schnittstelle für Callbacks
-export([init/1, handle_cast/2, handle_call/3,
         start/1, calc_reset/1, calc_inc/2, calc_mult/2, calc_div/2, calc_get/1]).

start(InitialValue) ->
    {ok, Pid} = gen_server:start(?MODULE, InitialValue, []),
    Pid.
                           %  ^^^^^^^^^^^^ wird zum  Argument von init
% macht einen neuen Prozess, ruft dort init auf, startet Schleife, die
% Nachrichten empfängt

-record(state, {initial :: number(), current :: number()}).

-record(reset, {}).
-record(inc, {increment :: number()}).
-record(mult, {factor :: number()}).
-record(divide, {divisor :: number()}).
-record(get, {}).

calc_reset(Pid) -> gen_server:cast(Pid, #reset{}).
calc_inc(Pid, Increment) -> gen_server:cast(Pid, #inc{increment = Increment}).
calc_mult(Pid, Factor) -> gen_server:cast(Pid, #mult{factor = Factor}).
calc_div(Pid, Divisor) -> gen_server:cast(Pid, #divide{divisor = Divisor}).
calc_get(Pid) -> gen_server:call(Pid, #get{}).

-type message() :: #reset{} | #inc{} 
                 | #mult{} | #divide{} | #get{}.

-spec update_calc_state(#state{}, message()) -> #state{}.
update_calc_state(#state{initial = Initial}, #reset{}) ->
    #state{initial = Initial, current = Initial};
update_calc_state(#state{initial = Initial, current = Current}, #inc{increment = Increment}) ->
    #state{initial = Initial, current = Current + Increment};
update_calc_state(#state{initial = Initial, current = Current}, #mult{factor = Factor}) ->
    #state{initial = Initial, current = Current * Factor};
update_calc_state(#state{initial = Initial, current = Current}, #divide{divisor = Divisor}) ->
    #state{initial = Initial, current = Current / Divisor}.

% muß auch noch Nachricht zurückschicken:
% #get{} ist anders als die anderen
% wird nicht benötigt, da es immer bei handle_call aufschlägt:
% update_calc_state(N, #get{}) -> N.

% InitialN kommt von gen_server:start
init(InitialN) -> {ok, #state{initial = InitialN, current = InitialN}}. % gibt initialen Zustand zurück

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

handle_call(#get{}, _From, N) ->
    #state{current = Current} = N,
    {reply, Current, Current}.