# Manual.make

MANGROFF = $(addprefix $(outdir)/, $(addsuffix .$(SECTION)))

local-install: $(MANGROFFS)
	-$(INSTALL) -d $(mandir)/man$(SECTION)
	$(INSTALL) -m 644 $(MANGROFFS) $(mandir)/man$(SECTION)


local-uninstall:
	(cd  $(mandir)/man$(SECTION)/; rm -f $(MANGROFF))




