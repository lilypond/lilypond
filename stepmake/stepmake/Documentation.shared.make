# stepmake/Documentation.shared.make
# urg: stuff shared between yodl/lily/stepmake

# could/should do this for bin/Makefile make/Makefile too

### below here is shared between lily/yodl/Documentation and topdocs...

AT_FILES = $(BLURBS) # 
at-dir = $(doc-dir)/
at-ext = .in

include $(stepdir)/Documentation.vars.make
include $(stepdir)/Documentation_rules.make

do-doc: $(OUTTXT_FILES) 

# ugh. FIXME
ifeq ($(strip $(README_TOP_FILES)),)
readme-top_FILES-html:
else

readme-top_FILES-html:
	for i in $(README_TOP_FILES); do \
	  $(SHELL) $(step-bindir)/text2html.sh $(outdir)/$$i.txt $(outdir)/$$i.html; \
	  $(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) --index=$(depth)/../index.html $(outdir)/$$i.html; \
	done
endif

local-WWW:  readme-top_FILES-html $(OUTHTML_FILES) $(OUTREADME_HTML_FILES) 

doc: do-doc

# what to do here?
ifneq ($(strip $(INFO_FILES)),)

INFOINSTALL=$(MAKE) INSTALLATION_OUT_DIR=$(infodir) depth=$(depth) INSTALLATION_OUT_FILES="$(INFO_FILES)" -f $(stepdir)/Install_outfiles.sub.make $@

$(outdir)/$(package).info: check-texinfo-deps $(OUTTEXINFO_FILES)
	$(MAKEINFO) -o $@ $(outdir)/topinfo.texinfo

localinstall: # $(INFO_FILES)
	-$(INSTALL) -d $(infodir)
	$(INFOINSTALL)
localuninstall:
	$(INFOINSTALL)

endif

