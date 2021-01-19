-module(intro_mike).
-export([double/1, double/2, state/1, typical/1,
    safe_divide/2, dogs_per_leg/1, t1/0, t2/0, minutes_since_midnight/1]).

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

% Ein Gürteltier hat folgende Eigenschaften:
% - tot oder lebendig
% - Gewicht
-record(dillo, {liveness :: dead | alive, weight :: number()}).

% Lebendiges Gürteltier, 10kg
d1() -> #dillo{liveness = alive, weight = 10}.

% Totes Gürteltier, 12kg
d2() -> #dillo{liveness = dead, weight =12}.

