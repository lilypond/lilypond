depth = ..

NAME = documentation
SUBDIRS=user bibliography pictures topdocs misc windows
STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly
LILYPOND_BOOK_FLAGS=--extra-options '-e "(set-lily-option (quote internal-type-checking) \#t)"'
README_TOP_FILES=NEWS DEDICATION CHANGES 
EXTRA_DIST_FILES=

include $(depth)/make/stepmake.make 

default: local-doc

test-reg:
	rm -f out-www/regression-test.html out-www/regression-test.texi 
	$(MAKE) out=www out-www/regression-test.html

local-WWW: copy-for-me txt-to-html $(outdir)/regression-test.ps.gz $(outdir)/regression-test.html footify

copy-for-me:
	$(foreach a, $(README_TOP_FILES), cp ../$(a) $(outdir)/$(a).txt && ) true

OUT_TXT_FILES = $(addprefix $(outdir)/, $(addsuffix .txt, $(README_TOP_FILES)))

# cd $(outdir) rule gets bit hairy for --srcdir configure builds
txt-to-html:
	$(PYTHON) $(step-bindir)/text2html.py $(OUT_TXT_FILES)
	$(footify) $(addprefix $(outdir)/, $(addsuffix .html, $(README_TOP_FILES)))
