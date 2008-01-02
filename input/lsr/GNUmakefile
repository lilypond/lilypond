depth = ../..

# urg, can't name itely templates foo.itely.in because of standard rule

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly
EXTRA_DIST_FILES += README $(call src-wildcard,*.snippet-list)

include $(depth)/make/stepmake.make

IN_ITELY_FILES = $(call src-wildcard,*-intro.itely)
GENERATED_ITELY_FILES = $(IN_ITELY_FILES:%-intro.itely=$(outdir)/%.itely)

$(outdir)/%.itely: %-intro.itely %.snippet-list
	xargs $(PYTHON) $(buildscript-dir)/lys-to-tely.py --name=$@ --template=$< < $(filter %.snippet-list, $^)

$(outdir)/snippets.texi: snippets.tely $(GENERATED_ITELY_FILES)
	$(PYTHON) $(LILYPOND_BOOK) $(LILYPOND_BOOK_INCLUDES) --process='$(LILYPOND_BINARY) $(LILYPOND_BOOK_INCLUDES)' --output=$(outdir) --format=$(LILYPOND_BOOK_FORMAT) $(LILYPOND_BOOK_FLAGS) $<

	$(MAKEINFO) -I$(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split --no-headers $<

$(outdir)/snippets/index.html: $(outdir)/snippets.texi $(OUT_PNG_IMAGES) $(OUT_EPS_IMAGES)
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(outdir) --output=$(dir $@) --css-include=$(top-src-dir)/Documentation/texinfo.css --html $<

local-WWW: $(outdir)/snippets.pdf $(outdir)/snippets/index.html $(outdir)/snippets-big-page.html
