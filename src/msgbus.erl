-module(msgbus).

-export([create/0, create_channel/2, channel_subscribe/1, channel_unsubscribe/1, channel_publish/2]).


create() ->
	register(msgbus, spawn(?MODULE, start, [])).

start() ->
	loop([{all, []}]).

loop(ChannelConfig) ->
	receive
		{subscribe, From, Channel} ->
		{unsubscribe, From, Channel} ->
		{publish, From, Msg, Opt} ->
		{broadcast, From, Msg} ->
		{}
		{'EXIT'} ->
	end.
