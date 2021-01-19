-module(intro_mike).
-export([double/1, double/2, state/1]).

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
typical(State) ->
    if
        State == fest -> -20;
        State == fluessig -> 18;
        true -> 300
    end.