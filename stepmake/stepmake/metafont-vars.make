
MF_FILES := $(wildcard *.mf)
FONT_FILES = $(wildcard *[0-9].mf)
EXTRA_DIST_FILES += $(MF_FILES)
MF_TFM_FILES = $(addprefix $(outdir)/, $(FONT_FILES:.mf=.tfm))
MF_DVI_FILES = $(addprefix $(outdir)/, $(FONT_FILES:.mf=.dvi))
MF_LOG_FILES = $(addprefix $(outdir)/, $(FONT_FILES:.mf=.log))
DVI_FILES += $(MF_DVI_FILES)
TFM_FILES += $(MF_TFM_FILES)

# XPM_MODE=sun
# XPM_RESOLUTION=85

# XPM_MODE=declarge
# XPM_RESOLUTION=100

XPM_MODE=ibmvga
XPM_RESOLUTION=110



