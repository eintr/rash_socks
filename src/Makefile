include ../config.mk

sources=queuectl.erl  simple_tcp_server.erl  socks4.erl rash_socks.erl shortlist.erl config.erl cidr.erl log.erl

beams=$(sources:.erl=.beam)

all: deps rash_socks_start $(beams)

%.beam: %.erl
	erlc $^

deps: config.hrl

config.hrl: config.hrl.tmpl
	@echo "Generating $@"
	@echo $(PREFIX) | sed 's/\//\\\//g' > /tmp/~rash_socks_make.template
	@sed "s/__PREFIX__/`cat /tmp/~rash_socks_make.template`/g" $< > $@
	@rm -f /tmp/~id_service_make.template

rash_socks_start: rash_socks_start.tmpl ../config.mk
	@echo "Generating $@"
	@echo $(PREFIX) | sed 's/\//\\\//g' > /tmp/~rash_socks_make.template
	@sed "s/__PREFIX__/`cat /tmp/~rash_socks_make.template`/g" $< > $@
	@rm -f /tmp/~id_service_make.template
	@chmod a+x $@

install:
	cp *.beam ../dist/ebin/
	cp rash_socks.conf ../dist/conf/

clean:
	rm -f $(beams) rash_socks_start config.hrl

