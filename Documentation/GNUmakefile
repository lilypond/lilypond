depth = ..

NAME = documentation
SUBDIRS=user bibliography pictures topdocs ntweb misc
STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond mudela

README_TOP_FILES=NEWS DEDICATION CHANGES TODO
EXTRA_DIST_FILES=

include $(depth)/make/stepmake.make 

default: local-doc

local-WWW: copy-for-me $(outdir)/regression-test.ps.gz $(outdir)/regression-test.html
# we want footers even if website builds (or is built) partly
	$(MAKE) footify

copy-for-me:
	$(foreach a, $(README_TOP_FILES),cp ../$(a) $(outdir)/$(a).txt && ) true

