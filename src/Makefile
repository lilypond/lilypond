default:
	$(MAKE) -C ..

DISTFILES=Makefile $(mycc) $(othersrc)
dist:
	ln $(DISTFILES) $(DDIR)/$(SUBDIR)

TAGS: $(mycc)
	etags -CT $(mycc) 

clean:
	rm -f $(gencc)