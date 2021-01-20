-module(calc_server).

-behaviour(gen_server).

% Schnittstelle für Callbacks
% Hier eine Implementierung des Interface gen_server

-record(reset, {}).
-record(inc, {increment :: number()}).
-record(mult, {factor :: number()}).
-record(divide, {divisor :: number()}).
-record(get, {pid :: pid()}).

-type message