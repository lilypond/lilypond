
YO_FILES := $(wildcard *.yo)
OUTYO_FILES = $(addprefix $(outdir)/,$(YO_FILES))

OUTTXT_FILES = $(OUTYO_FILES:.yo=.txt) $(OUTIN_FILES:.yo=.txt)
OUTHTML_FILES = $(OUTYO_FILES:.yo=.html) $(OUTIN_FILES:.yo=.html)
