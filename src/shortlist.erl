-module(shortlist).

-export([new/1, append/2, getlog/1]).

new(MaxLength) ->
	{?MODULE, MaxLength, []}.

append(Item, {?MODULE, MaxLength, Log}) ->
	if
		length(Log) == MaxLength ->
			[_|NewLog] = Log ++ [Item],
			{?MODULE, MaxLength, NewLog};
		true ->
			{?MODULE, MaxLength, Log ++ [Item]}
	end.

getlog({?MODULE, _, Log}) ->
	Log.

