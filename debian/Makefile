# debian/Makefile

depth = ..

NAME= debian

include $(depth)/make/Stepmake.make 

CONF_FILES=changelog copyright postinst postrm rules watch 
OUTCONF_FILES=$(addprefix $(outdir)/, $(basename $(CONF_FILES))) $(OUTIN_FILES)
OUT_DIST_FILES = $(OUTIN_FILES)

default: $(OUTCONF_FILES)

BLURBS=BLURB
AT_FILES = $(BLURBS) # 
at-dir = $(outdir)/
at-ext =

EXTRA_DIST_FILES = README.Debian TODO $(CONF_FILES) $(OUT_DIST_FILES) control.foka

$(outdir)/%: %
	rm -f $@
	ln $< $@

$(outdir)/control: $(addprefix $(outdir)/, $(BLURBS))

$(outdir)/%: $(doc-dir)/%.in
	rm -f $@
	cat $< | sed 's%^% %' > $@

localdist: default
