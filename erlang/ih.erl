-module(ih).
-export([double/1, double/2, state/1, typical/1,
    safe_divide/2, dogs_per_leg/1,
    ist_niedlich/1, noon/0, minutes_since_midnight/1, t1/0, t2/0, t3/0,
    g1/0, g2/0, g3/0, g4/0, guerteltier_ueberfahren/1, guerteltier_fuettern/2,
    p1/0, p2/0, tier_ueberfahren/1,
    list_sum/1, list_mult/1, guerteltier_gewichte/1, glist/0,
    format_process/0, format_process_loop/0, run_process/0
]).

-spec double(number()) -> number().
double(X) -> X * 2.

-spec double(number(), number()) -> number().
double(X, Y) -> X * Y * 2.

% Aggregatzustand von Wasser
-spec state(number()) -> fest | fluessig | gasfoermig.
state(Temp) ->
    if 
        Temp =< 0 -> fest;
        Temp < 100 -> fluessig;
        Temp >= 100 -> gasfoermig % alternativ true
    end.

% ungleich /=, gleich ==

-spec typical(fest | fluessig | gasfoermig) -> number().
% typical(State) ->
%     if
%         State == fest -> -20;
%         State == fluessig -> 10;
%         true -> 120
%     end.

typical(fest) -> -20;
typical(fluessig) -> -20;
typical(gasfoermig) -> 120.

safe_divide(X, Y) ->
    if
        Y == 0 -> {error, divide_by_zero};
        true -> {ok, X / Y}
    end.

dogs_per_leg(Legs) ->
    case safe_divide(Legs, 4) of
        {ok, Dogs} -> Dogs;
        {error, _Exception} -> io:format("this can't happen")
    end.

% Haustiere: Hunde, Katzen, Schlangen
% Funktion, die ermittelt, ob ein Haustier niedlich ist
-spec ist_niedlich(hund | katze | schlange) -> boolean().
ist_niedlich(hund) -> {ok, false};
ist_niedlich(katze) -> {ok, true};
ist_niedlich(schlange) -> {ok, false}.

% Zusammengesetze Daten
-record(time, {hour :: 0..23, minute :: 0..59}).

noon() -> #time{hour = 12, minute = 24}.

t1() -> #time{hour = 12, minute = 24}.
t2() -> #time{hour = 0, minute = 24}.
t3() -> #time{hour = 12, minute = 0}.

-spec minutes_since_midnight(#time{}) -> number().
minutes_since_midnight(Time) ->
    % Time#time.hour * 60 + Time#time.minute.
    Min1 = Time#time.hour * 60,
    Min1 + Time#time.minute.

-record(guerteltier, {zustand :: tot | lebendig, gewicht :: 10..100}).

g1() -> #guerteltier{zustand = lebendig, gewicht = 25}.
g2() -> #guerteltier{zustand = tot, gewicht = 12}.
g3() -> #guerteltier{zustand = lebendig, gewicht = 13}.
g4() -> #guerteltier{zustand = tot, gewicht = 15}.
glist() -> [g1(), g2(), g3(), g4()].

-spec guerteltier_ueberfahren(#guerteltier{}) -> #guerteltier{}.
guerteltier_ueberfahren(#guerteltier{zustand = _Zustand, gewicht = Gewicht}) -> #guerteltier{zustand = dead, gewicht = Gewicht}.

% Gürteltier füttern, lebendig: nimmt zu
-spec guerteltier_fuettern(#guerteltier{}, number()) -> #guerteltier{}.
guerteltier_fuettern(#guerteltier{zustand = lebendig, gewicht = Gewicht}, Futter) -> #guerteltier{zustand = lebendig, gewicht = Gewicht + Futter};
guerteltier_fuettern(#guerteltier{zustand = tot, gewicht = _Gewicht} = G, _Futter) -> G.
    % #guerteltier{zustand = tot, gewicht = Gewicht}.

-record(papagei, {satz :: string(), gewicht :: 1..10}).

p1() -> #papagei{satz = "Hallo!", gewicht = 2}.
p2() -> #papagei{satz = "Ballo!", gewicht = 3}.

-type tier() :: #guerteltier{} | #papagei{}.

-spec tier_ueberfahren(tier()) -> tier().
tier_ueberfahren(#guerteltier{gewicht = Gewicht}) ->
    #guerteltier{zustand = dead, gewicht = Gewicht};
tier_ueberfahren(#papagei{gewicht = Gewicht}) ->
    #papagei{satz = "", gewicht = Gewicht}.

% Liste
% - die leere Liste oder
% - eine Cons-Liste, bestehend aus dem ersten Element und der restlichen Liste
% [1 | 2,3] - sprich Cons
% [ H | T ] - Head, Tail

-spec list_sum(list(number())) -> number().
list_sum([]) -> 0;
list_sum([First|Rest]) ->
    First
    + list_sum(Rest).

-spec list_mult(list(number())) -> number().
list_mult([]) -> 1; % Letzter Aufruf ist das neutrale Element, hier der Multiplikation
list_mult([First|Rest]) ->
    First
    * list_mult(Rest).

% -spec guerteltier_gewichte(list(#guerteltier{}) -> list(number()).
% guerteltier_gewichte([]) -> [];
% guerteltier_gewichte([#guerteltier{gewicht = Gewicht}|Rest]) ->
%     [Gewicht | guerteltier_gewichte(Rest)].

% -spec list_map(func((A) -> B)), list(A)) -> list(B).
% list_map(_F, []) -> [];
% list_map(F, [First|Rest]) ->

% Prozesse
% spawn() startet einen neuen Prozess
% spawn(fun() -> io:format("Hello world.\n") end).
% Pid = ...
% Pid ! "Das ist eine Message."
format_process() ->
    spawn(fun() ->
        receive
            Message -> io:format(Message)
            after 10000 -> % ms
                io:formt("timeout~n")
        end
    end).

format_process_loop() ->
        receive
            Message -> io:format(Message)
            after 10000 -> % ms
                io:formt("timeout~n")
        end,
        format_process_loop().

run_process() ->
    spawn(fun format_process_loop/0).


