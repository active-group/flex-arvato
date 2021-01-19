-module(intro_mike).
-export([double/1, double/2, state/1, typical/1,
    safe_divide/2, dogs_per_leg/1]).

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