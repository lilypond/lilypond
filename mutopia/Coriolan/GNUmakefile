# mutopia/Makefile

depth = ../..

# do Coriolan if you have plenty RAM / CPU / quotum
examples=

examples= 
LOCALSTEPMAKE_TEMPLATES=mutopia

include $(depth)/make/stepmake.make

parts=$(patsubst %.ly,%,$(wildcard *-part.ly))

tarball=coriolan
mutopia-examples=coriolan $(parts)
mutopia-letter=$(mutopia-examples:%=out-letter/%.ps.gz)

mutopia:
	$(MAKE) examples="$(mutopia-examples)" PAPERSIZE=letter local-WWW $(mutopia-letter)

