-module(socks4).

-export([start/0, start/1]).

-export([socks4_callback/2, relay_socket/4]).

-include_lib("kernel/include/inet.hrl").

-define(DEFAULT_PORT, 1080).

start() ->
	start(?DEFAULT_PORT).

start(Port) ->
	queuectl:create(),
	Pid = simple_tcp_server:create(Port, {?MODULE, socks4_callback, []}),
	loop(Pid).

loop(Pid) ->
	receive
		_ -> ok
	end,
	loop(Pid).

socks4_callback(Client_socket, Arg) ->
	%io:format("Got a connection.\n"),
	socks4_protocol(Client_socket, Arg).

socks4_protocol(Client_socket, Arg) ->
	socks4_protocol(recv_request, Client_socket, Arg, []).

socks4_protocol(recv_request, Client_socket, Arg, Context) ->
	%io:format("recv_request"),
	{ok, Head} = gen_tcp:recv(Client_socket, 8),
	case Head of
		<<4:8, 1:8,
				Dport:16/big-unsigned-integer,
				A:8, B:8, C:8, D:8 >> ->
			% OK
			Uid = case get_seg(Client_socket) of
				{error, Reason} ->
					socks4_protocol(term, Client_socket, Arg, Context++[{log, Reason}]);
				Ret ->
					Ret
			end,
			case {A, B, C, D} of
				{0, 0, 0, 1} ->
					DomainName = get_seg(Client_socket),
					%io:format("DomainName=~s\n", [DomainName]),
					socks4_protocol(name_resolv, Client_socket, Arg, Context++[{dport, Dport}, {domainname, DomainName}, {uid, Uid}]);
				Daddr ->
					%io:format(": Dport=~b, Daddr=~p, Uid=~p\n", [Dport, Daddr, Uid]),
					socks4_protocol(try_connect, Client_socket, Arg, Context++[{dport, Dport}, {daddr, Daddr}, {uid, Uid}])
			end;
		<<VN:8, CD:8, Dport:16/big-unsigned-integer, A:8, B:8, C:8, D:8, Uid:16/big-unsigned-integer>> ->
			io:format("Unknown request: VN=~b, CD=~b, Dport=~b, Daddr=~b.~b.~b.~b, Uid=~p\n", [VN, CD, Dport, A, B, C, D, Uid])
	end;

socks4_protocol(name_resolv, Client_socket, Arg, Context) ->
	case inet_res:gethostbyname(context_get(domainname, Context)) of
		{ok, Hostent} ->
			[Daddr|_] = Hostent#hostent.h_addr_list,
			socks4_protocol(try_connect, Client_socket, Arg, Context++[{daddr, Daddr}]);
		{error, Reason} ->
			socks4_protocol(term, Client_socket, Arg, Context++[{error, Reason}])
	end;

socks4_protocol(try_connect, Client_socket, Arg, Context) ->
	Dsockaddr = {context_get(daddr, Context), context_get(dport, Context)},
	io:format("try_connect to ~p ... ", [Dsockaddr]),
	case queuectl:connect_request(Dsockaddr) of
		true ->
			case gen_tcp:connect(context_get(daddr, Context), context_get(dport, Context), [{active, false}, binary], 3000) of
				{ok, Socket} ->
					queuectl:connect_ok(context_get(Dsockaddr, Context)),
					io:format("ok!\n"),
					socks4_protocol(send_grant, Client_socket, Arg, Context++[{server, Socket}]);
				{error, eagain} ->
					queuectl:connect_timeout(context_get(Dsockaddr, Context)),
					io:format("timedout!\n"),
					socks4_protocol(send_reject, Client_socket, Arg, Context++[{reason, "timedout"}]);
				{error, Reason} ->
					queuectl:connect_fail(context_get(Dsockaddr, Context)),
					io:format("failed!\n"),
					socks4_protocol(send_reject, Client_socket, Arg, Context++[{reason, Reason}])
			end;
		false ->
			io:format("Connection to ~p rejected by queue ctl.\n", [context_get(daddr, Context)]),
			socks4_protocol(send_reject, Client_socket, Arg, Context++[{reason, "Queue control reject"}])
	end;
	
socks4_protocol(send_grant, Client_socket, Arg, Context) ->
	%io:format("send_grant\n"),
	Dport = context_get(dport, Context),
	{A, B, C, D} = context_get(daddr, Context),
	Reply = <<0:8, 90:8, Dport:16/big-unsigned-integer, A:8, B:8, C:8, D:8>>,
	gen_tcp:send(Client_socket, Reply),
	socks4_protocol(relay, Client_socket, Arg, Context);

socks4_protocol(send_reject, Client_socket, Arg, Context) ->
	%io:format("send_reject\n"),
	Dport = context_get(dport, Context),
	{A, B, C, D} = context_get(daddr, Context),
	Reply = <<0:8, 92:8, Dport:16/big-unsigned-integer, A:8, B:8, C:8, D:8>>,
	gen_tcp:send(Client_socket, Reply),
	socks4_protocol(term, Client_socket, Arg, Context);

socks4_protocol(relay, Client_socket, Arg, Context) ->
	%io:format("relaying: ~p\n", [Context]),
	PID1 = spawn(?MODULE, relay_socket, [Client_socket, context_get(server, Context), Arg, self()]),
	PID2 = spawn(?MODULE, relay_socket, [context_get(server, Context), Client_socket, Arg, self()]),
	socks4_protocol(relay_loop, Client_socket, Arg, Context ++ [{worker1, PID1}, {worker2, PID2}]);

socks4_protocol(relay_loop, Client_socket, Arg, Context) ->
	%io:format("."),
	receive
		{relayerror, Reason} ->
			io:format("relayerror: ~p\n", [Reason]),
			socks4_protocol(term, Client_socket, Arg, Context);
		_ ->
			socks4_protocol(term, Client_socket, Arg, Context)
	end;

socks4_protocol(term, _Client_socket, _Arg, _Context) ->
	%io:format("terminating\n"),
	% TODO: Do some log if needed.
	ok.

relay_socket(Socket1, Socket2, Arg, PPID) ->
	case gen_tcp:recv(Socket1, 0) of
		{ok, Data} ->
			%io:format("Received: ~p, ", [Data]),
			case gen_tcp:send(Socket2, Data) of
				ok ->
					%io:format("Sent: ~p\n", [Data]),
					relay_socket(Socket1, Socket2, Arg, PPID);
				{error, Reason} ->
					io:format("Sent error: ~s\n", [Reason]),
					PPID ! {relayerror, "Send error: "++Reason}
			end;
		{error, Reason} ->
			PPID ! {relayerror, "Receive error: "++Reason}
	end.

get_seg(Socket) ->
	get_seg(Socket, []).

get_seg(Socket, Seg) ->
	case gen_tcp:recv(Socket, 1) of
		{ok, <<0:8>>} ->
			Seg;
		{ok, Byte} ->
			get_seg(Socket, Seg++binary_to_list(Byte));
		{error, Reason} ->
			{error, Reason}
	end.

context_get(Key, Context) ->
	case lists:keyfind(Key, 1, Context) of
		{Key, Value} ->
			Value;
		_ ->
			false
	end.

