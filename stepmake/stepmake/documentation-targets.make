
default:


local-WWW: $(OUTHTML_FILES) footify

local-web:
	$(MAKE) CONFIGSUFFIX=www local-WWW

footify:
	$(footify) $(sort $(wildcard $(outdir)/*.html out/*.html out-www/*.html))

deep-footify:
	$(deep-footify) $(sort $(wildcard $(outdir)/*/*.html))

INFO_INSTALL_FILES = $(wildcard $(addsuffix *, $(INFO_FILES)))

INFOINSTALL=$(MAKE) INSTALLATION_OUT_DIR=$(infodir) depth=$(depth) INSTALLATION_OUT_FILES="$(INFO_INSTALL_FILES)" -f $(stepdir)/install-out.sub.make

localinstall: install-info
localuninstall: uninstall-info

install-info: $(INFO_FILES)
	-$(INSTALL) -d $(infodir)
	$(INFOINSTALL) localinstall

uninstall-info:
	$(INFOINSTALL) localuninstall
	-rmdir $(infodir)