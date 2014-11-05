-module(log).

-export([create/1, log/2, log/3]).

-import(config, [config/2]).

-author("牛海青<nhf0424@gmail.com>").

-export([log_center_start/2]).

create(Config) ->
	Path = config(log_file, Config),
	case file:open(Path, [append, {encoding, utf8}, sync]) of
		{ok, LogFile} ->
			register(log_center, spawn_link(?MODULE, id_log_start, [LogFile, Config]));
		{error, Reason} ->
			{error, Reason}
	end.

log(Level, String) ->
	id_log ! {log, Level, string:strip(String), []}.

log(Level, Format, ArgList) ->
	id_log ! {log, Level, string:strip(Format), ArgList}.

log_center_start(LogFile, Config) ->
	put(log_debug,	{"DEBUG", 0}),
	put(log_info,	{"INFO", 1}),
	put(log_notice,	{"NOTICE", 2}),
	put(log_warning,	{"WARNING", 3}),
	put(log_error,	{"ERROR", 4}),
	put(log_critical,	{"CRITICAL", 5}),

	LeastLevel = config(log_level, Config),
	{_, LeastLevel_val} = get(LeastLevel),
	log(log_info, "id_log started at level ~p.", [LeastLevel]),
	log_center_loop(LogFile, LeastLevel_val, Config).

log_center_loop(LogFile, LeastLevel, Config) ->
	receive
		{log, Level, Format, ArgList} ->
			{LogLevelString, LogValue} = get(Level),
			if
				LogValue>=LeastLevel ->
					io:format(LogFile, prepend_time(io_lib:format(LogLevelString++": "++Format, ArgList)), []),
					log_center_loop(LogFile, LeastLevel, Config);
				true ->	% Drop low level logs.
					log_center_loop(LogFile, LeastLevel, Config)
			end;
		{'EXIT'} ->
			ok
	end.

prepend_time(String) ->
	timestamp()++" -- "++String++"\n".

timestamp() ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
	io_lib:format("~p-~p-~p ~p:~p:~p", [Year, Month, Day, Hour, Minute, Second]).

