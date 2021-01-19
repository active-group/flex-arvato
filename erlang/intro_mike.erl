-module(intro_mike).
-export([double/1, double/2, state/1, typical/1,
    safe_divide/2, dogs_per_leg/1, t1/0, t2/0, minutes_since_midnight/1,
    d1/0, d2/0, run_over_dillo/1, p1/0, p2/0, run_over_animal/1,
    animal_weight/1, list_sum/1, list_product/1,
    animal_weights/1, run_over_animals/1, highway/0, list_map/2]).

% Atome: mike, stefan, error
% Liste: [1,2,3]
% Tupel: {1, "mike", false, foo}
% Zeichenkette: "mike"
% Binary: <<"Mike">>

% Variablen: fangen mit Großbuchstaben an
% Kleinbuchstaben: Atom
-spec double(number()) -> number().
double(X) -> X * 2.

-spec double(number(), number()) -> number().
double(X, Y) -> X * Y * 2.

% Aggregatzustand von Wasser
-spec state(number()) -> fest | fluessig | gas.
state(Temp) ->
    if
        Temp == 0 -> fest;
        Temp < 100 -> fluessig;
        true -> gas
    end.

% Typische Temperatur zum Aggregatzustand
-spec typical(fest | fluessig | gas) -> number().
% Pattern Matching
typical(fest) -> -20;
typical(fluessig) -> 18;
typical(gas) -> 300.

% typical(State) ->
%    if
%        State == fest -> -20;
%        State == fluessig -> 18;
%        true -> 300
%    end.

-spec safe_divide(number(), number()) -> {error, divide_by_zero} | {ok, number()}.
safe_divide(X, Y) ->
    if
        Y == 0 -> {error, divide_by_zero};
        true -> {ok, X / Y}
    end.

dogs_per_leg(Legs) ->
    case safe_divide(Legs, 4) of
        % Pattern: 2-Tupel, erstes Element: Atom ok, zweites Element
        {ok, Dogs} -> Dogs;
        {error, divide_by_zero} -> io:format("this can't happen")
    end.

% 3 Sorten Haustiere: Hunde, Katzen, Schlange
% Schreibe eine Funktion, die herausbekommt, ob ein Haustier niedlich ist!

% Zusammengesetzte Daten:
% - "besteht aus"
% - "hat folgende Eigenschaften"

% Eine Uhrzeit besteht aus:
% - Stunde
% - Minute
-record(time, {hour :: 0..23, minute :: 0..59}).

t1() -> #time{hour = 12, minute = 24}. % 12 Uhr 24
t2() -> #time{hour = 12, minute = 0}. % 12 Uhr 00

% int x = 10;

% Minuten seit Mitternacht
-spec minutes_since_midnight(#time{}) -> number().
minutes_since_midnight(Time) ->
    Minutes1 = Time#time.hour * 60,
    Minutes2 = (Minutes1 + Time#time.minute),
    Minutes2.

% Zustand eines Gürteltiers zu einem bestimmten Zeitpunkt

% Ein Gürteltier hat folgende Eigenschaften:
% - tot oder lebendig
% - Gewicht
-record(dillo, {liveness :: dead | alive, weight :: number()}).

% Ein Papagei hat folgende Eigenschaften:
% - Satz
% - Gewicht
-record(parrot, {sentence :: string(), weight :: number()}).

% Ein Tier ist eins der folgenden:
% - ein Gürteltier ODER
% - ein Papagei
% Fallunterscheidung, gemischte Daten
-type animal() :: #dillo{} | #parrot{}.

% Lebendiges Gürteltier, 10kg
d1() -> #dillo{liveness = alive, weight = 10}.

% Totes Gürteltier, 12kg
d2() -> #dillo{liveness = dead, weight =12}.

p1() -> #parrot{sentence = "Hello!", weight = 1}.
-spec p2() -> animal().
p2() -> #parrot{sentence = "Idiot!", weight = 0.5}.

% Gewicht hochziehen:
% -record tanimal{ weight :: number(), kind :: ...}

% Gürteltier überfahren
% class Dillo { void runOver() { this.alive = false; } }
-spec run_over_dillo(#dillo{}) -> #dillo{}.
run_over_dillo(#dillo{weight = Weight}) ->
    #dillo{liveness = dead, weight = Weight}.

-spec animal_weight(animal()) -> number().
animal_weight(#dillo{weight = Weight}) -> Weight;
animal_weight(#parrot{weight = Weight}) -> Weight.

-spec run_over_animal(animal()) -> animal().
run_over_animal(#dillo{weight = Weight}) ->
    #dillo{liveness = dead, weight = Weight};
run_over_animal(#parrot{weight = Weight}) ->
    #parrot{sentence = "", weight = Weight}.

highway() -> [d1(), d2(), p1(), p2()].

% Eine Liste ist eins der folgenden:
% - die leere Liste ODER
% - eine Cons-Liste, bestehend aus dem ersten Element 
%   und der Rest-Liste
%                ^^^^^ Selbstreferenz

% x + n = n + x = x
% n: neutrales Element der Addition

% x * n = n * x = x
% n: neutrale Element der Multiplikation

% Alle Elemente einer Liste aufsummieren
-spec list_sum(list(number())) -> number().
list_sum([]) -> 0;
list_sum([First|Rest]) ->
    First % das erste Element
    + list_sum(Rest). % die Summe der restlichen Summe
 
 % Alle Elemente einer Liste aufmultiplizieren
-spec list_product(list(number())) -> list(number()).
list_product([]) -> 1;
list_product([First|Rest]) ->
    First % das erste Element
    * list_product(Rest). % das Produkt der restlichen Summe

-spec animal_weights(list(animal())) -> list(number()).
animal_weights([]) -> [];
animal_weights([First|Rest]) ->
    [ animal_weight(First) | animal_weights(Rest) ].

-spec run_over_animals(list(animal())) -> list(animal()).
run_over_animals([]) -> [];
run_over_animals([First|Rest]) ->
    [run_over_animal(First) | run_over_animals(Rest) ].

% Abstraktion über animal_weights und run_over_animals
-spec list_map(fun((A) -> B), list(A)) -> list(B).
list_map(_F, []) -> [];
list_map(F, [First|Rest]) ->
    [ F(First) | list_map(F, Rest) ].

% rev([1,2,3]) ... rev([2,3])

-spec rev(list(A)) -> list(A).
rev([]) -> [];
rev([First|Rest]) ->
    First
    rev(Rest)