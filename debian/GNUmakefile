# debian/Makefile

depth = ..

NAME= debian
BLURBS=BLURB
AT_FILES = $(BLURBS) # 
at-dir = $(outdir)/
at-ext =

EXTRA_DIST_FILES = README.Debian TODO $(CONF_FILES) $(OUT_DIST_FILES) control.foka
CONF_FILES=changelog copyright preinst postinst postrm rules watch 
OUTCONF_FILES=$(addprefix $(outdir)/, $(basename $(CONF_FILES))) $(OUTIN_FILES)
OUT_DIST_FILES = $(OUTIN_FILES)

STEPMAKE_TEMPLATES = yolily-debian
include $(depth)/make/stepmake.make 

