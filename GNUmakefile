

NAME=ikebana
VERSION=0.0
PY_FILES=$(wildcard *.py)
ALL_FILES=$(PY_FILES) GNUmakefile server.ly README

DISTNAME=$(NAME)-$(VERSION)

dist:
	mkdir $(DISTNAME)
	ln $(ALL_FILES) $(DISTNAME)/
	tar cfz $(DISTNAME).tar.gz $(DISTNAME)/
	rm -rf $(DISTNAME)





