-module(socks4).

-export([socks4_callback/2, relay_socket/4]).

-import(config, [config/2]).
-import(log, [log/2, log/3]).

-include_lib("kernel/include/inet.hrl").

-define(DEFAULT_PORT, 1080).

socks4_callback(Client_socket, [Config]) ->
	log(log_debug, "Got connection."),
	socks4_protocol(Client_socket, Config).

socks4_protocol(Client_socket, Config) ->
	socks4_protocol(recv_request, Client_socket, Config, []).

socks4_protocol(recv_request, Client_socket, Config, Context) ->
	%io:format("recv_request"),
	{ok, Head} = gen_tcp:recv(Client_socket, 8),
	case Head of
		<<4:8, 1:8,
				Dport:16/big-unsigned-integer,
				A:8, B:8, C:8, D:8 >> ->
			% OK
			Uid = case get_seg(Client_socket) of
				{error, Reason} ->
					socks4_protocol(term, Client_socket, Config, Context++[{log, Reason}]);
				Ret ->
					Ret
			end,
			case {A, B, C, D} of
				{0, 0, 0, 1} ->
					DomainName = get_seg(Client_socket),
					%io:format("DomainName=~s\n", [DomainName]),
					socks4_protocol(name_resolv, Client_socket, Config, Context++[{dport, Dport}, {domainname, DomainName}, {uid, Uid}]);
				Daddr ->
					%io:format(": Dport=~b, Daddr=~p, Uid=~p\n", [Dport, Daddr, Uid]),
					socks4_protocol(try_connect, Client_socket, Config, Context++[{dport, Dport}, {daddr, Daddr}, {uid, Uid}])
			end;
		<<VN:8, CD:8, Dport:16/big-unsigned-integer, A:8, B:8, C:8, D:8, Uid:16/big-unsigned-integer>> ->
			log(log_error, "Unknown socks header: VN=~b, CD=~b, Dport=~b, Daddr=~b.~b.~b.~b, Uid=~p\n", [VN, CD, Dport, A, B, C, D, Uid])
	end;

socks4_protocol(name_resolv, Client_socket, Config, Context) ->
	case inet_res:gethostbyname(config(domainname, Context)) of
		{ok, Hostent} ->
			[Daddr|_] = Hostent#hostent.h_addr_list,
			socks4_protocol(try_connect, Client_socket, Config, Context++[{daddr, Daddr}]);
		{error, Reason} ->
			socks4_protocol(term, Client_socket, Config, Context++[{error, Reason}])
	end;

socks4_protocol(try_connect, Client_socket, Config, Context) ->
	Dsockaddr = {config(daddr, Context), config(dport, Context)},
	%io:format("try_connect to ~p ... ", [Dsockaddr]),
	case queuectl:connect_request(Dsockaddr) of
		true ->
			case gen_tcp:connect(config(daddr, Context), config(dport, Context), [{active, false}, binary], 3000) of
				{ok, Socket} ->
					queuectl:connect_ok(Dsockaddr),
					%io:format("ok!\n"),
					socks4_protocol(send_grant, Client_socket, Config, Context++[{server, Socket}]);
				{error, timeout} ->
					queuectl:connect_timeout(Dsockaddr),
					%io:format("timedout!\n"),
					socks4_protocol(send_reject, Client_socket, Config, Context++[{reason, "timedout"}]);
				{error, Reason} ->
					queuectl:connect_fail(Dsockaddr),
					%io:format("failed!\n"),
					socks4_protocol(send_reject, Client_socket, Config, Context++[{reason, Reason}])
			end;
		false ->
			log(log_info, "Connection to ~p rejected by queue ctl.", [Dsockaddr]),
			socks4_protocol(send_reject, Client_socket, Config, Context++[{reason, "Queue control reject"}])
	end;
	
socks4_protocol(send_grant, Client_socket, Config, Context) ->
	%io:format("send_grant\n"),
	Dport = config(dport, Context),
	{A, B, C, D} = config(daddr, Context),
	Reply = <<0:8, 90:8, Dport:16/big-unsigned-integer, A:8, B:8, C:8, D:8>>,
	gen_tcp:send(Client_socket, Reply),
	socks4_protocol(relay, Client_socket, Config, Context);

socks4_protocol(send_reject, Client_socket, Config, Context) ->
	%io:format("send_reject\n"),
	Dport = config(dport, Context),
	{A, B, C, D} = config(daddr, Context),
	Reply = <<0:8, 92:8, Dport:16/big-unsigned-integer, A:8, B:8, C:8, D:8>>,
	gen_tcp:send(Client_socket, Reply),
	socks4_protocol(term, Client_socket, Config, Context);

socks4_protocol(relay, Client_socket, Config, Context) ->
	%io:format("relaying: ~p\n", [Context]),
	{_, ServerRTO, ServerSTO} = config:addr_config({config(daddr, Context), config(dport, Context)}, Config),
	PID1 = spawn(?MODULE, relay_socket, [Client_socket, config(server, Context), {-1, -1, ServerSTO}, self()]),
	PID2 = spawn(?MODULE, relay_socket, [config(server, Context), Client_socket, {-1, ServerRTO, -1}, self()]),
	socks4_protocol(relay_loop, Client_socket, Config, Context ++ [{worker1, PID1}, {worker2, PID2}]);

socks4_protocol(relay_loop, Client_socket, Config, Context) ->
	Worker1 = config(worker1, Context),
	Worker2 = config(worker2, Context),
	if
		(Worker1 == over) and (Worker2 == over) ->
			socks4_protocol(term, Client_socket, Config, Context);
		true ->
			receive
				{over, Worker1, Reason} ->
					log(log_info, "Relayer C->S is over: ~p\n", [Reason]),
					socks4_protocol(term, Client_socket, Config, lists:keyreplace(worker1, 1, Context, {worker1, over}));
				{over, Worker2, Reason} ->
					log(log_info, "Relayer S->C is over: ~p\n", [Reason]),
					socks4_protocol(term, Client_socket, Config, lists:keyreplace(worker2, 1, Context, {worker1, over}));
				_ ->
					socks4_protocol(term, Client_socket, Config, Context)
			end
	end;

socks4_protocol(term, _Client_socket, _Config, _Context) ->
	%io:format("terminating\n"),
	% TODO: Do some log if needed.
	ok.

relay_socket(Socket1, Socket2, {_CTO, 0, _STO}=Config, PPID) ->
	case gen_tcp:recv(Socket1, 0) of
		{ok, Data} ->
			%io:format("Received: ~p, ", [Data]),
			case gen_tcp:send(Socket2, Data) of
				ok ->
					%io:format("Sent: ~p\n", [Data]),
					relay_socket(Socket1, Socket2, Config, PPID);
				{error, closed} ->
					PPID ! {over, self()};
				{error, Reason} ->
					io:format("Sent error: ~s\n", [Reason]),
					PPID ! {over, self(), Reason}
			end;
		{error, closed} ->
			PPID ! {over, self(), "EOF"};
		{error, Reason} ->
			PPID ! {over, self(), Reason}
	end;
relay_socket(Socket1, Socket2, {_CTO, RTO, _STO}=Config, PPID) ->
	case gen_tcp:recv(Socket1, 0, RTO) of
		{ok, Data} ->
			%io:format("Received: ~p, ", [Data]),
			case gen_tcp:send(Socket2, Data) of
				ok ->
					%io:format("Sent: ~p\n", [Data]),
					relay_socket(Socket1, Socket2, Config, PPID);
				{error, closed} ->
					PPID ! {over, self(), "EOF"};
				{error, Reason} ->
					io:format("Sent error: ~s\n", [Reason]),
					PPID ! {over, self(), Reason}
			end;
		{error, closed} ->
			PPID ! {over, self(), "EOF"};
		{error, Reason} ->
			PPID ! {over, self(), Reason}
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

