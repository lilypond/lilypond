
default:

do-doc: $(OUTTXT_FILES) 


local-WWW: $(OUTHTML_FILES)
	echo $^ > $(depth)/wwwlist

doc: do-doc


# what to do here?
ifeq (a,b) 

$(outdir)/$(package).info: $(outdir)/topinfo.texinfo $(OUTTEXINFO_FILES)
	$(MAKEINFO) --force -o $@ $(outdir)/topinfo.texinfo


INFOINSTALL=$(MAKE) INSTALLATION_OUT_DIR=$(infodir) depth=$(depth) INSTALLATION_OUT_FILES="$(INFO_FILES)" -f $(stepdir)/install-out.sub.make $@

localinstall: # $(INFO_FILES)
	-$(INSTALL) -d $(infodir)
	$(INFOINSTALL)
localuninstall:
	$(INFOINSTALL)

endif

