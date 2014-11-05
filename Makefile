include config.mk

DIRS=src

all: deps
	for d in $(DIRS) ; do   \
		make -C $$d  all ;     \
	done

deps:
	for d in $(DIRS) ; do	\
		make -C $$d	deps	;	\
	done

install:
	for d in $(DIRS) ; do	\
		make -C $$d	install	;	\
	done
	[ -r $(PREFIX)/conf/rash_socks.conf ] && rm -f dist/conf/rash_socks.conf
	cp -r dist/* $(PREFIX)

clean:
	for d in $(DIRS) ; do	\
		make -C $$d	clean	;	\
	done

