# Documentation files
AT_FILES = $(BLURBS) # 
at-dir = $(doc-dir)/
at-ext = .in

footify=$(PYTHON) $(step-bindir)/add-html-footer.py --name $(PACKAGE_NAME) --version $(TOPLEVEL_VERSION) --footer $(depth)/Documentation/footer.html.in

#
YO_FILES := $(wildcard *.yo)
POD_FILES := $(wildcard *.pod)
TEXINFO_FILES := $(wildcard *.texinfo)
OUTYO_FILES = $(addprefix $(outdir)/,$(YO_FILES))
OUTPOD_FILES = $(addprefix $(outdir)/,$(POD_FILES))
OUTTEXINFO_FILES = $(addprefix $(outdir)/,$(TEXINFO_FILES))

#
ALL_DOC_FILES = $(POD_FILES) $(TEXINFO_FILES) $(YO_FILES)
OUTTXT_FILES = $(OUTYO_FILES:.yo=.txt) $(OUTIN_FILES:.yo=.txt)
OUTHTML_FILES = $(OUTYO_FILES:.yo=.html) $(OUTIN_FILES:.yo=.html)

OUTREADME_TXT_FILES=$(addprefix $(outdir)/,$(addsuffix .txt, $(README_TOP_FILES)))
OUTREADME_HTML_FILES=$(addprefix $(outdir)/,$(addsuffix .html, $(README_TOP_FILES)))

ALL_SOURCES += $(ALL_DOC_FILES)
