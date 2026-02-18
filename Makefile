.PHONY: build test clean install uninstall

build:
	gerbil build

test: build
	gerbil test pcre2/pcre2-test.ss

clean:
	gerbil clean

install: build
	cd /tmp && gerbil pkg link -g gerbil-pcre $(CURDIR)
	cd /tmp && gerbil pkg build -g gerbil-pcre

uninstall:
	cd /tmp && gerbil pkg unlink -g gerbil-pcre
