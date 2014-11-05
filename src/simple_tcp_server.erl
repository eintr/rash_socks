-module(simple_tcp_server).

-export([create/2]).

-export([thr_monitor/2, thr_worker/2, worker_wrapper/2]).

-author("牛海青").

%-include_lib("kernel/include/file.hrl").

create(Port, CallBack) when is_number(Port) ->
	create({"0.0.0.0", Port, []}, CallBack);
create({Addr, Port, Opt}, {_Module, ServerFunction, Arg}=CallBack) when is_number(Port), is_list(Opt), is_atom(ServerFunction), is_list(Arg) ->
	spawn(?MODULE, thr_monitor, [{Addr, Port, Opt}, CallBack]).

thr_monitor({Addr, Port, Opt}, CallBack) ->
	%io:format("Monitor thread created.~n", []),
	Pid_worker = spawn_link(?MODULE, thr_worker, [{Addr, Port, Opt}, CallBack]),
	%io:format("Worker thread created.~n", []),
	monitor(process, Pid_worker),
	thr_monitor_loop(Pid_worker, {{Addr, Port, Opt}, CallBack}).

thr_monitor_loop(Pid_worker, {Sockaddr, CallBack}) ->
	receive
		{'DOWN', _, process, _Pid, _Info} ->
			Pid_worker2 = spawn_link(?MODULE, thr_worker, [Sockaddr, CallBack]),
			%io:format("Worker thread recreated.~n", []),
			thr_monitor_loop(Pid_worker2, {Sockaddr, CallBack});
		{'EXIT', _Pid, Reason} ->
			%io:format("Worker thread exit normally: ~p.~n", [Reason]),
			exit(Reason);
		kill ->
			Pid_worker ! {'EXIT', self(), "Normal exit."},
			thr_monitor_loop(Pid_worker, {Sockaddr, CallBack});
		_ ->
			%io:format("Unknown message: ~p .~n", [Msg]),
			thr_monitor_loop(Pid_worker, {Sockaddr, CallBack})
	end.

thr_worker({Addr, Port, Opt}, CallBack) when is_number(Port) ->
	{ok, IP} = inet:parse_ipv4_address(Addr),
	{ok, ListenSocket} = gen_tcp:listen(Port, [{ip, IP}, {reuseaddr, true}, {packet, 0}, {active, false}, binary] ++ Opt),
	%io:format("ListenSocket created.~n", []),
	accept_loop(ListenSocket, CallBack).	% Never return.

accept_loop(ListenSocket, CallBack) ->
	{ok, ClientSocket} = gen_tcp:accept(ListenSocket),
	%{ok, {A, P}} = inet:peername(ClientSocket),
	%io:format("Got a connection: ~p:~p~n", [A, P]),
	Pid = spawn(?MODULE, worker_wrapper, [CallBack, ClientSocket]),
	gen_tcp:controlling_process(ClientSocket, Pid),
	accept_loop(ListenSocket, CallBack).

worker_wrapper({Module, ServerFunction, Arg}, ClientSocket) ->
	Module:ServerFunction(ClientSocket, Arg),
	gen_tcp:close(ClientSocket).

