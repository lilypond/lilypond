
# MF_FILES := $(wildcard *.mf)
# FONT_FILES = $(wildcard *[0-9].mf)
# EXTRA_DIST_FILES += $(MF_FILES)
MP_PFA_FILES = $(addprefix $(outdir)/, $(FONT_FILES:.mf=.pfa))
PFA_FILES += $(MP_PFA_FILES)

