
default:

# UGH. 
include $(stepdir)/www-targets.make

local-WWW: $(OUTHTML_FILES) 

# why isn't this in texinfo-targets?
INFO_INSTALL_FILES = $(wildcard $(addsuffix *, $(INFO_FILES)))

# should we call install-info?
INFOINSTALL=$(MAKE) INSTALLATION_OUT_DIR=$(DESTDIR)$(package_infodir) depth=$(depth) INSTALLATION_OUT_FILES="$(INFO_INSTALL_FILES)" -f $(stepdir)/install-out.sub.make

local-install: install-info
local-uninstall: uninstall-info
local-install-info:
local-uninstall-info:
install-info: local-install-info
uninstall-info: local-uninstall-info

install-info: $(INFO_FILES)
	-$(INSTALL) -d $(DESTDIR)$(package_infodir)
	$(INFOINSTALL) local-install
	-install-info --info-dir=$(infodir) $(outdir)/$(package).info

uninstall-info:
	-install-info --info-dir=$(infodir) --remove $(outdir)/$(package).info
	$(INFOINSTALL) local-uninstall
	-rmdir $(infodir)
