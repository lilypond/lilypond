depth = ../..

NAME = documentation

STEPMAKE_TEMPLATES=documentation  texinfo

TEXTS =AIMS $(wildcard CHANGES-*[0-9])  $(wildcard ANNOUNCE-*[0-9]) $(wildcard NEWS-*[0-9]) interview
EXTRA_DIST_FILES = $(TEXTS)

include $(depth)/make/stepmake.make 

default: local-doc

#urg default local-WWW target uses footify before its time; 
# must add footify with txt-to-html target
local-WWW:  $(addprefix $(outdir)/, $(addsuffix .txt, $(TEXTS))) $(OUT_HTMLFILES) txt-to-html

copy-for-me:
	$(foreach a, $(README_TOP_FILES), cp ../$(a) $(outdir)/$(a).txt && ) true

$(outdir)/%.txt: %
	cp $< $@

txt-to-html:
	$(foreach a, $(TEXTS),\
		echo "<html><body><xmp>" > $(outdir)/$(a).html;\
		cat $(outdir)/$(a).txt >> $(outdir)/$(a).html;\
		echo "</xmp></body></html>" >> $(outdir)/$(a).html\
	&& ) true
	$(footify) $(addprefix $(outdir)/, $(addsuffix .html, $(TEXTS)))
