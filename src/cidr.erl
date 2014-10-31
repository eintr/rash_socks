-module(cidr).

-export([match/2, prefix_parse/1]).

prefix_parse(Str) ->
	Tokens = string:tokens(Str, "/"),
	case length(Tokens) of
		1 ->
			[Prefix] = Tokens,
			case inet:parse_address(Prefix) of
				{ok, Addr} ->
					{Addr, 32};
				_ ->
					throw("Illegal prefix")
			end;
		2->
			[P, L] = Tokens,
			{Stat, Prefix} = inet:parse_address(P),
			Len = list_to_integer(L),
			if
				Stat == error ->
					throw("Illegal prefix");
				Len>32 ->
					throw("Illegal prefix length");
				Len<0 ->
					throw("Illegal prefix length");
				true ->
					{Prefix, Len}
			end;
		_->
			throw("Unknown format")
	end.

match({A1, B1, C1, D1}, {{A2, B2, C2, D2}, Len}) ->
	Num  = (A1*256*256*256 + B1*256*256 + C1*256 + D1) bsr (32-Len),
	Masq = (A2*256*256*256 + B2*256*256 + C2*256 + D2) bsr (32-Len),
	if
		Masq == Num ->
			true;
		true ->
			false
	end.

