depth = ..

NAME = documentation
SUBDIRS=user bibliography pictures topdocs  misc
STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly
LILYPOND_BOOK_FLAGS=--extra-options '-e "(ly:set-option (quote internal-type-checking) \#t)"'
README_TOP_FILES= DEDICATION THANKS ChangeLog
EXTRA_DIST_FILES= texinfo.css

include $(depth)/make/stepmake.make

default: local-doc

local-WWW: copy-for-me txt-to-html $(outdir)/index.html

local-WWW-clean: deep-WWW-clean

deep-WWW-clean:
	rm -rf $(outdir)/wiki-dump

copy-for-me:
	$(foreach a, $(README_TOP_FILES), cp $(top-src-dir)/$(a) $(outdir)/$(a).txt && ) true

OUT_TXT_FILES = $(addprefix $(outdir)/, $(addsuffix .txt, $(README_TOP_FILES)))

# cd $(outdir) rule gets bit hairy for --srcdir configure builds
txt-to-html:
	$(PYTHON) $(step-bindir)/text2html.py $(OUT_TXT_FILES)
