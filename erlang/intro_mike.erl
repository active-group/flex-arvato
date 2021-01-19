-module(intro_mike).
-export([double/1, double/2]).

% Atome: mike, stefan, error
% Liste: [1,2,3]
% Tupel: {1, "mike", false, foo}
% Zeichenkette: "mike"
% Binary: <<"Mike">>

% Variablen: fangen mit Großbuchstaben an
% Kleinbuchstaben: Atom
double(X) -> X * 2.

double(X, Y) -> X * Y * 2.