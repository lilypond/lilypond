.PHONY: install-doc uninstall-doc distclean top-doc
install-doc:
uninstall-doc:

distclean: clean doc-clean test-clean log-clean
	$(MAKE) local-distclean

local-distclean:
	rm -f config.hh config.make GNUmakefile \
		config.cache config.status config.log
	rm -rf autom4te.cache
	rm -rf $(outdir)

GNUmakefile: GNUmakefile.in
	$(MAKE) INFILE=$< OUTFILE=$@ -f $(stepdir)/automatically-generated.sub.make

$(package-icon):
	$(MAKE) -C Documentation/logo icon


top-doc:
	$(MAKE) -C Documentation/topdocs/ README_TOP_FILES="$(TOPDOC_FILES)" txt-files


$(README_TXT_FILES): top-doc

local-clean:

install:
	$(LOOP)
ifeq ($(strip $(SRCMAKE)),)
	$(MAKE) final-install
endif

local-help:
	@echo "  config          rerun configure"
	@echo "  dist            roll tarball: $(depth)/$(outdir)/$(distname).tar.gz"
	@echo "  distclean       make clean, doc-clean, test-clean, log-clean and"
	@echo "                   also remove configure output"
	@echo "  po              make new translation Portable Object database"
	@echo "  po-replace      do po-update and replace catalogs with msgmerged versions"
	@echo "  po-update       update translation Portable Object database"
	@echo
	@echo "  install-doc     install website documentation in"
	@echo "                    (webdir=$(webdir))"
	@echo "                    and Info documentation with images"
	@echo "  uninstall-doc   remove installed documentation with images"
	@echo "  info            build Info documentation with images"
	@echo "  install-info    install Info documentation with images"
	@echo
	@echo "Some more targets are available for testing changes:"
	@echo "  test-baseline"
	@echo "  check"
	@echo "  test-redo"
	@echo "  test-clean"
	@echo
	@echo "  For more information on these targets, see"
	@echo "    \`Verify regression tests' in the Contributor's Guide."
	@echo

