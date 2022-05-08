default: man

local-install: install-help2man

install-help2man: man
	-$(INSTALLPY) -d $(DESTDIR)$(mandir)/man1
	$(INSTALLPY) -c -m 644 $(HELP2MAN_GROFFS) $(DESTDIR)$(mandir)/man1

man: $(HELP2MAN_GROFFS)

local-uninstall: uninstall-help2man

uninstall-help2man:
	$(foreach a, $(HELP2MAN_GROFFS), rm -f $(DESTDIR)$(mandir)/man1/$(notdir $(a)) && ) true
	-rmdir $(DESTDIR)$(mandir)/man1






