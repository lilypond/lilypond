# mutopia/J.S.Bach/Solo-Cello-Suites/Makefile

depth = ../../..

EXTRA_DIST_FILES=

examples=scsii-cello
LOCALSTEPMAKE_TEMPLATES=mutopia

include $(depth)/make/stepmake.make

tarball=solo-cello-suite-ii
mutopia-examples=scsii-cello scsii-viola
mutopia-letter=$(mutopia-examples:%=out-letter/%.ps.gz)

mutopia:
	$(MAKE) examples="$(mutopia-examples)" PAPERSIZE=letter local-WWW $(mutopia-letter)

