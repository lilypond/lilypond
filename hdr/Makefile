default:
	$(MAKE) -C ..
DISTFILES=Makefile $(allhdr)
dist:
	ln $(DISTFILES) $(DDIR)/$(SUBDIR)


TAGS: $(hdr) 
	$(TAGSACTION) $^

clean:
	rm -f parser.hh
	rm -f midi-parser.hh
