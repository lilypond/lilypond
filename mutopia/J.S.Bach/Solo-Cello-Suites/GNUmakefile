# mutopia/J.S.Bach/Solo-Cello-Suites/Makefile

depth = ../../..

EXTRA_DIST_FILES=

examples=scsii-cello
LOCALSTEPMAKE_TEMPLATES=mutopia

include $(depth)/make/stepmake.make

tarball=solo-cello-suite-ii

mutopia:
	$(MAKE) examples="scsii-cello scsii-viola" PAPERSIZE=letter local-WWW out-letter/scsii-cello.ps.gz out-letter/scsii-viola.ps.gz

