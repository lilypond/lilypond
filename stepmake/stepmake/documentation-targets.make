
default:

do-doc: $(OUTTXT_FILES) 

# ugh. FIXME
ifeq ($(strip $(README_TOP_FILES)),)
readme-top_FILES-html:
readme-top_FILES-txt:
else

readme-top_FILES-txt:
	$(foreach i, $(README_TOP_FILES), \
	  cp $(depth)/$(i) $(outdir)/$(i).txt && ) true

readme-top_FILES-html:
	$(foreach i, $(README_TOP_FILES), \
	  $(SHELL) $(step-bindir)/text2html.sh $(outdir)/$(i).txt $(outdir)/$(i).html && \
	  $(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) --index=$(depth)/../index.html $(outdir)/$(i).html && ) true
endif

local-WWW: readme-top_FILES-txt readme-top_FILES-html $(OUTHTML_FILES) $(OUTREADME_HTML_FILES) 
	echo $^ > $(depth)/wwwlist

doc: do-doc

$(outdir)/$(package).info: $(outdir)/topinfo.texinfo $(OUTTEXINFO_FILES)
	$(MAKEINFO) -o $@ $(outdir)/topinfo.texinfo

# what to do here?
ifneq ($(strip $(INFO_FILES)),)

INFOINSTALL=$(MAKE) INSTALLATION_OUT_DIR=$(infodir) depth=$(depth) INSTALLATION_OUT_FILES="$(INFO_FILES)" -f $(stepdir)/install-outfiles.sub.make $@

localinstall: # $(INFO_FILES)
	-$(INSTALL) -d $(infodir)
	$(INFOINSTALL)
localuninstall:
	$(INFOINSTALL)

endif

