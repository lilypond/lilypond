default: man

local-install: install-help2man

install-help2man: man
	-$(INSTALL) -d $(mandir)/man1
	$(foreach a,  $(HELP2MAN_GROFFS), \
		$(INSTALL) -m 644 $(a) $(mandir)/man1 && ) true

man: $(HELP2MAN_GROFFS)

local-uninstall: uninstall-help2man

uninstall-help2man:
	$(foreach a, $(notdir $(MANGROFFS)), rm -f $(a) && ) true






