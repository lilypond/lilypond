default:
	$(MAKE) -C ..
DISTFILES=Makefile $(hdr)
dist:
	ln $(DISTFILES) $(DDIR)/$(SUBDIR)


TAGS: $(hdr) 
	etags -CT $(hdr) 

clean:
	rm -f parser.hh