depth = ..

NAME = documentation
SUBDIRS=user bibliography pictures topdocs misc windows
STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly

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

txt-to-html:
	$(foreach a, $(README_TOP_FILES), \
		echo "<html><body><xmp>" > $(outdir)/$(a).html;\
		cat $(outdir)/$(a).txt >> $(outdir)/$(a).html;\
		echo "</xmp></body></html>" >> $(outdir)/$(a).html\
	&& ) true
	$(footify) $(addprefix $(outdir)/, $(addsuffix .html, $(README_TOP_FILES)))
