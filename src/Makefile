default:
	$(MAKE) -C ..

win32:
	$(MAKE) -C .. Windows_NT

DISTFILES=Makefile $(allcc) $(othersrc)
dist:
	ln $(DISTFILES) $(DDIR)/$(SUBDIR)

TAGS: $(allcc)
	$(TAGSACTION) $^

clean:
	rm -f $(gencc)
