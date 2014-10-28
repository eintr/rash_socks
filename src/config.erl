-module(config).

-export([config/2]).

config(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        {ok, Value} -> Value;
        false -> false
    end.

