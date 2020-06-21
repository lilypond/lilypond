.PHONY: install-doc uninstall-doc distclean top-doc
install-doc:
uninstall-doc:

distclean: clean doc-clean test-clean
	$(MAKE) local-distclean

local-distclean:
	rm -f config.hh config.make GNUmakefile \
		config.cache config.status config.log
	rm -rf autom4te.cache
	rm -rf $(outdir)

$(package-icon):
	$(MAKE) -C Documentation/logo icon


local-clean:

install:
	$(LOOP)
ifeq ($(strip $(SRCMAKE)),)
	$(MAKE) final-install
endif

local-help:
	@echo "  config          rerun configure"
	@echo "  dist            roll tarball: $(depth)/$(outdir)/$(distname).tar.gz"
	@echo "  distclean       make clean, doc-clean, test-clean and"
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
	@echo "  test-clean"
	@echo
	@echo "  For more information on these targets, see"
	@echo "    \`Verify regression tests' in the Contributor's Guide."
	@echo
