default:
	$(MAKE) -C ..

dist:
	ln Makefile $(hdr) $(DDIR)/$(HEADERDIR)

TAGS: $(hdr) 
	etags -CT $(hdr) 

clean:
	rm -f parser.hh