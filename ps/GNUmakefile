# ps/GNUmakefile

depth = ..

TEMPLATES = install-out install

PS_FILES = $(call src-wildcard,*.ps)
GEN_PS_FILES = $(outdir)/encodingdefs.ps

INSTALLATION_DIR = $(local_lilypond_datadir)/ps
INSTALLATION_FILES = $(PS_FILES)

INSTALLATION_OUT_DIR = $(INSTALLATION_DIR)
INSTALLATION_OUT_FILES = $(GEN_PS_FILES)

include $(depth)/make/lilypond.make

$(outdir)/encodingdefs.ps: $(buildscript-dir)/generate-encodings.py
	$(call ly_progress,Making,$@,< $(notdir $<))
	$(PYTHON) $< > $@

$(outdir)/%.ps: | %.ps
	ln -sf $(abs-src-dir)/$*.ps $@

LINKED_PS_FILES = $(addprefix $(outdir)/,$(PS_FILES))

default: $(GEN_PS_FILES) $(LINKED_PS_FILES)
