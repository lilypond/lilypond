# debian/Makefile

depth = ..

NAME = debian
at-dir = $(outdir)/
at-ext =

CONF_FILES = changelog control copyright doc-base emacsen-startup \
	preinst postinst postrm prerm rules watch
EXTRA_DIST_FILES = README.Debian TODO $(CONF_FILES)  \
	control.foka control.potato
#OUTCONF_FILES = $(addprefix $(outdir)/, $(basename $(CONF_FILES))) $(OUTIN_FILES)
OUTCONF_FILES = $(OUTIN_FILES)
OUT_DIST_FILES = $(OUTIN_FILES)

STEPMAKE_TEMPLATES = debian
include $(depth)/make/stepmake.make 

