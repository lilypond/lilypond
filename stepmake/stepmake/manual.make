# Manual.make

MANGROFF = $(addprefix $(outdir)/, $(addsuffix .$(SECTION)))

localinstall: $(MANGROFFS)
	-$(INSTALL) -d $(mandir)/man$(SECTION)
	$(INSTALL) -m 644 $(MANGROFFS) $(mandir)/man$(SECTION)


localuninstall:
	(cd  $(mandir)/man$(SECTION)/; rm -f $(MANGROFF))




