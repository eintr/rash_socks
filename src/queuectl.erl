-module(queuectl).

-export([create/0]).

create() ->
	create([]).

create(Config) ->
	register(qdict, spawn_link(?MODULE, start, [Config])).

start(Config) ->
	precess_flag(priority, high),
	loop(dict:new()).

loop(Dict) ->
	receive
		{addr2pid, From, Addr} ->
			case dict:find() of
				{ok, Pid} ->
					From ! {ok, Pid},
					loop(Dict);
				error ->
					From ! {error, "Not found"}
			end;
		{register, From, {Addr, Pid}} ->
			loop(dict:append(Addr, Pid, Dict));
		{unregister, From, Addr} ->
			loop(dict:erase(Addr, Dict))
	end.

connect_request(Key) ->
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_request, self()},
			receive
				Reply -> Reply
			end;
		{error, Reason} ->
			qdict ! {register, self(), {Key, spawn(?MODULE, addr_server_start, [Key])}},
			true;
	end.

addr_server_start(Server) ->
	addr_server_loop(Server, {0, 0, 10000, []}).

addr_server_loop(Server, {Queue, EstDelay, MaxDelay}) ->
	receive
		{set, From, {maxdelay, Value}} ->
			NewQueue = adjust_queue(Queue, EstDelay, Value),
			addr_server_loop(Key, {NewQueue, EstDelay, Value});
		{connect_request, From} ->
			if
				EstDelay < 0 ->		% Server is unavailable.
					From ! false;
					addr_server_loop(Server, {Queue, EstDelay, MaxDelay})
				EstDelay*length(Queue) > MaxDelay ->	% Server is too busy.
					From ! false;
					addr_server_loop(Server, {Queue, EstDelay, MaxDelay})
				length(Queue) == 0 ->
					From ! true     % Try
					addr_server_loop(Server, {Queue++[{From, timestamp()}], EstDelay, MaxDelay});
				_ ->
					From ! true		% Server is OK
					addr_server_loop(Server, {Queue++[{From, timestamp()}], EstDelay, MaxDelay})
			end;
		{connect_ok, From} ->
			{ok, {Pid, Timestamp}} = lists:find(From, 1, Queue),
			Delay = timestamp() - Timestamp,
			addr_server_loop(Server -- [{Pid, Timestamp}], {Queue, (EstDelay+Delay)/2, MaxDelay});
		{connect_fail, From} ->
			{ok, {Pid, Timestamp}} = lists:find(From, 1, Queue),
			Delay = timestamp() - Timestamp,
			addr_server_loop(Server -- [{Pid, Timestamp}], {Queue, EstDelay*2, MaxDelay});
	end,
	addr_server_loop(Server, Cfg).

adjust_queue(Qlen, EstDelay, MaxDelay, Queue) ->
	F = fun ({SocksPid, Timestamp}=Elm, Acc) ->
		if
			timestamp() - Timestamp > MaxDelay ->	% Expired.
				exit(SocksPid ,"Connecting was timed out"),
				false;
			true ->
				true
	end,
	lists:filter(F, [], Queue).

timestamp() ->
	{MegaSecs, Secs, MicroSecs} = now(),
	(MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

