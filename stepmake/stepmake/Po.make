

####
#### UGH!
new-po:
	if test -r $(po-dir); then \
	  rm -f $(po-dir)/$(outdir)/$(package).po; \
	  touch $(po-dir)/$(outdir)/$(package).po; \
	fi

ifeq ($(strip $(depth)),.)
po: new-po
	$(LOOP)
localpo:
	@true
else
po: localpo
	$(LOOP)
ALL_PO_SOURCES = $(ALL_C_SOURCES) $(wildcard $(outdir)/*.hh) $(wildcard $(outdir)/*.cc)
localpo:
ifneq ($(strip $(ALL_PO_SOURCES)),)
	@echo $(ALL_PO_SOURCES)
	xgettext --c++ --default-domain=$(package) --join \
	 --output-dir=$(po-dir)/$(outdir) --add-comments \
	 --keyword=_ --keyword=_f $(ALL_PO_SOURCES)
endif
endif


po-update: po
	$(MAKE) -C $(po-dir) po-update

show-po-changes:
	$(MAKE) -C $(po-dir) show-po-changes

