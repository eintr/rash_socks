sources=queuectl.erl  simple_tcp_server.erl  socks4.erl rash_socks.erl shortlist.erl

beams=$(sources:.erl=.beam)

all: $(beams)

%.beam: %.erl
	erlc $^

clean:
	rm -f $(beams)

