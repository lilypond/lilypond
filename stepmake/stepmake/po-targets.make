


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
ALL_PO_SOURCES = $(ALL_C_SOURCES) $(ALL_CC_SOURCES) $(PYTHON_SCRIPTS_IN) $(wildcard $(outdir)/*.hh) $(wildcard $(outdir)/*.cc)
localpo:
ifneq ($(strip $(ALL_PO_SOURCES)),)
	@echo $(ALL_PO_SOURCES)
	xgettext --c++ --default-domain=$(package) --join \
	 --output-dir=$(po-dir)/$(outdir) --add-comments \
	 --keyword=_ --keyword=_f --keyword=_i $(ALL_PO_SOURCES)
endif
endif


po-update: po
	$(MAKE) -C $(po-dir) po-update

po-changes:
	$(MAKE) -C $(po-dir) po-changes

po-replace: po
	$(MAKE) -C $(po-dir) po-replace