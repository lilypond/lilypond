default:
	$(MAKE) -C ..

win32:
	$(MAKE) -C .. Windows_NT

DISTFILES=Makefile $(mycc) $(othersrc)
dist:
	ln $(DISTFILES) $(DDIR)/$(SUBDIR)

TAGS: $(mycc)
	etags -CT $(mycc) 

clean:
	rm -f $(gencc)
