-module(config).

-export([default/0, config/2, load_conf/1, addr_config/2]).

-define(DEFAULT_CONFIG, [
	{port, 10800},
	{addr, "0.0.0.0"},
	{admin_port, 8972},
	{shortlist_size, 256},
	{servers, [
		{'*', {2000, 3000, 2000}}	]}	]).

-include_lib("kernel/include/inet.hrl").

config(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        {Key, Value} -> Value;
        false -> false
    end.

addr_config(Address, Config) ->
	addr_config_find(Address, config(servers, Config)).

addr_config_find(_, []) ->
	{2000, 3000, 2000};
addr_config_find({Ip, Port}=Address, [{{Prefix, Port}, Conf}| Tail]) ->
	case cidr:match(Ip, Prefix) of
		true ->
			Conf;
		false ->
			addr_config_find(Address, Tail)
	end.

kvlist_merge([], Background) ->
	Background;
kvlist_merge([{K, V}|T], Background) ->
	kvlist_merge(T, lists:keystore(K, 1, Background, {K, V})).

default() ->
	?DEFAULT_CONFIG.

load_conf(Filename) ->
	{ok, Value} = file:script(Filename),
	%io:format("Raw: ~p\n", [Value]),
	Conf = kvlist_merge(Value, ?DEFAULT_CONFIG),
	%io:format("Combined with default: ~p\n", [Conf]),
	Servers_conf = server_conf_process(config(servers, Conf)),
	Ret = lists:keyreplace(servers, 1, Conf, {servers, Servers_conf}),
	%io:format("After process: ~p\n", [Ret]),
	Ret.

% Reserve these lines for supporting domain name in the future.
%inet_ptoerl({_A, _B, _C, _D}=Arg) ->
%	Arg;
%inet_ptoerl(Arg) when is_list(Arg)->
%	case inet:parse_ipv4_address(Arg) of
%		{ok, IPv4Address} ->
%			IPv4Address;
%		{error, _} ->
%			case inet:gethostbyname(Arg) of
%				{ok, Hostent} ->
%					[H|_] = Hostent#hostent.h_addr_list,
%					H;
%				{error, Err} ->
%					{error, Err}
%			end
%	end.

server_conf_process(List_from_conf_file) ->
	Parse = fun
		({'*', ServerConf}) ->
			{{cidr:prefix_parse("0.0.0.0/0"),  0}, ServerConf};
		({{'*', '*'}, ServerConf}) ->
			{{cidr:prefix_parse("0.0.0.0/0"),  0}, ServerConf};
		({{'*', Port}, ServerConf}) ->
			{{cidr:prefix_parse("0.0.0.0/0"),  Port}, ServerConf};
		({{Prefix_str, '*'}, ServerConf}) ->
			{{cidr:prefix_parse(Prefix_str),  0}, ServerConf};
		({{Prefix_str, Port}, ServerConf}) ->
			{{cidr:prefix_parse(Prefix_str),  Port}, ServerConf}
	end,
	ExtractLen = fun
		({{{Prefix, Len}, Port}, ServerConf}) ->
			{32-Len, {{Prefix, Len}, Port}, ServerConf}
	end,
	StripeLen = fun
		({_, Addr, Config})-> {Addr, Config}
	end,
	%io:format("Raw = ~p\n", [List_from_conf_file]),
	Tmp1 = lists:map(Parse, List_from_conf_file),
	%io:format("Parsed = ~p\n", [Tmp1]),
	Tmp2 = lists:map(ExtractLen, Tmp1),
	%io:format("Len raised = ~p\n", [Tmp2]),
	Tmp3 = lists:keysort(1, Tmp2),
	%io:format("Sorted = ~p\n", [Tmp3]),
	Striped = lists:map(StripeLen, Tmp3),
	%io:format("Striped = ~p\n", [Striped]),
	Striped.

