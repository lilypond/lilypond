depth = ..

NAME = documentation
SUBDIRS= user programmer bibliography pictures topdocs ntweb misc
STEPMAKE_TEMPLATES=documentation  texinfo

README_TOP_FILES=NEWS DEDICATION CHANGES TODO
EXTRA_DIST_FILES = 

include $(depth)/make/stepmake.make 

default: local-doc

local-WWW: copy-for-me

copy-for-me:
	$(foreach a, $(README_TOP_FILES),cp ../$(a) $(outdir)/$(a).txt && ) true

