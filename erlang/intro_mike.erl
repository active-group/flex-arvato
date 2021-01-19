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
state(Temp) ->
    if
        Temp < 0 -> fest;
        Temp < 100 -> fluessig;
        Temp >= 100 -> gas
    end.
