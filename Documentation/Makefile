PODS=README.pod CodingStyle.pod lilygut.pod lilyinput.pod
TXTS=$(PODS:.pod=.txt)
MANS=$(PODS:.pod=.1)
DOCFILES=$(PODS) Makefile 


default:
	$(MAKE) -C .. doc

doc: $(TXTS)

dist:
	ln $(DOCFILES) $(DDIR)/Documentation/
clean:
	rm -f $(TXTS) $(MANS)

%.txt: %.1
	groff -man -Tascii $< > $@

%.1: %.pod
	pod2man --center="LilyPond documentation" --section="0"\
		--release="LilyPond $(MAJVER).$(MINVER).$(PATCHLEVEL)" $< > $@

