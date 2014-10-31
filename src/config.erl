-module(config).

-export([default/0, config/2, tuple_merge/2, load_conf/1, server_conf_process/0]).

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

kvlist_merge([], Background) ->
	Background;
kvlist_merge([{K, V}|T], Background) ->
	kvlist_merge(T, lists:keystore(K, 1, Background, {K, V})).

tuple_merge(A, Bg) ->
	list_to_tuple(list_merge(tuple_to_list(A), tuple_to_list(Bg), [])).

list_merge(A, B) -> list_merge(A, B, []).
list_merge([], _, Res) ->
	Res;
list_merge(_, [], Res) ->
	Res;
list_merge(['*'|T1], [B|T2], Res) ->
	list_merge(T1, T2, Res++[B]);
list_merge([A|T1], ['*'|T2], Res) ->
	list_merge(T1, T2, Res++[A]);
list_merge([A|T1], [B|T2], Res) ->
	list_merge(T1, T2, Res++[A]).

default() ->
	?DEFAULT_CONFIG.

load_conf(Filename) ->
	{ok, Value} = file:script(Filename),
	kvlist_merge(Value, ?DEFAULT_CONFIG).

getaddrlist({A, B, C, D}, Port) ->
	[{{A, B, C, D}, Port}];
getaddrlist(Addr, Port) when is_list(Addr) ->
	case inet:parse_ipv4_address(Addr) of
		{ok, IPv4Address} ->
			{}
	end.

inet_ptoerl({_A, _B, _C, _D}=Arg) ->
	Arg;
inet_ptoerl(Arg) when is_list(Arg)->
	case inet:parse_ipv4_address(Arg) of
		{ok, IPv4Address} ->
			IPv4Address;
		{error, _} ->
			case inet:gethostbyname(Arg) of
				{ok, Hostent} ->
					[H|_] = Hostent#hostent.h_addr_list,
					H;
				{error, Err} ->
					{error, Err}
			end
	end.

server_conf_process() ->
	server_conf_process(
		[{{"10.210.74.190",80},{1000,1000,1000}},
		{{"10.75.12.60",80},{2000,1000,1000}},
		{{"0.0.0.0/0",'*'},{2000,3000,20000}}]).
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
	io:format("Raw = ~p\n", [List_from_conf_file]),
	Tmp1 = lists:map(Parse, List_from_conf_file),
	io:format("Parsed = ~p\n", [Tmp1]),
	Tmp2 = lists:map(ExtractLen, Tmp1),
	io:format("Len raised = ~p\n", [Tmp2]),
	Tmp3 = lists:keysort(1, Tmp2),
	io:format("Sorted = ~p\n", [Tmp3]),
	Tmp3.

