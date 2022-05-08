# scm/GNUmakefile

depth = ..

INSTALLATION_DIR=$(local_lilypond_datadir)/scm/lily
INSTALLATION_FILES=$(SCM_FILES)

# This assignment is lazy and uses the function src-wildcard that will only
# be defined when including the templates. The reason we need this here is the
# definition of local-po in po-targets.make, which checks that ALL_PO_SOURCES
# is not empty.
SCM_FILES = $(call src-wildcard,*.scm)

XGETTEXT_FLAGS = --language=Scheme
TEMPLATES = install po

include $(depth)/make/lilypond.make

TAGS_SOURCES += $(SCM_FILES)

# Sync with $(tree-share-prefix) in the top-level GNUmakefile.in!
tree-share-prefix = $(top-build-dir)/out/share/lilypond/current
guile-ccache = $(tree-share-prefix)/guile/ccache

# This rule depends on *all* .scm files because Guile is horribly bad at
# dependency tracking for macros. If any of the source files changes, we
# have to rebuild the entire bytecode to be sure.
$(outdir)/bytecode.dummy: compile.ly $(SCM_FILES)
	$(MAKE) clean
	$(RM) -r $(guile-ccache)
	mkdir -p $(outdir)
	GUILE_AUTO_COMPILE=1 $(LILYPOND_BINARY) $<
	for i in $$(find $(guile-ccache) -name "*.scm.go"); do \
		mv $$i $(outdir)/$$(basename $$i .scm.go).go; \
	done
	$(RM) -r $(guile-ccache)
	touch $@ $(outdir)/bytecode-default.dummy

.PHONY: bytecode
bytecode: $(outdir)/bytecode.dummy

# Due to Guile's problems figuring out if files need to be recompiled if macros
# are involved, make the default target delete *all* of them if any of the
# source files changes.
$(outdir)/bytecode-default.dummy: $(SCM_FILES)
	[ ! -f $(outdir)/lily.go ] || \
		echo "*** Changes in source .scm file detected, purging all compiled bytecode!"
	$(MAKE) clean
	mkdir -p $(outdir)
	touch $@

default: $(outdir)/bytecode-default.dummy

bytecode_installation_dir = $(local_lilypond_libdir)/ccache/lily

.PHONY: install-bytecode
install-bytecode:
	-$(INSTALLPY) -d $(DESTDIR)$(bytecode_installation_dir)
	$(INSTALLPY) -m 644 $(wildcard $(outdir)/*.go) $(DESTDIR)$(bytecode_installation_dir)/
