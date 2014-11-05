sources=queuectl.erl  simple_tcp_server.erl  socks4.erl rash_socks.erl shortlist.erl config.erl cidr.erl

beams=$(sources:.erl=.beam)

all: $(beams)

%.beam: %.erl
	erlc $^

install:
	cp *.beam ../dist/sbin/
	cp rash_socks.conf ../dist/conf/

clean:
	rm -f $(beams)

