PODS=README.pod CodingStyle.pod lilygut.pod lilyinput.pod error.pod\
	faq.pod index.pod language.pod
TXTS=$(PODS:.pod=.txt)
MANS=$(PODS:.pod=.1)
HTMLS=$(PODS:.pod=.html)
DISTFILES=$(PODS) Makefile lelie_logo.png make_website


default:
	$(MAKE) -C .. doc

doc: $(TXTS)

dist:
	ln $(DISTFILES) $(DDIR)/$(SUBDIR)
clean:
	rm -f $(TXTS) $(MANS)

%.txt: %.1
	groff -man -Tascii $< > $@

%.1: %.pod
	pod2man --center="LilyPond documentation" --section="0"\
		--release="LilyPond $(MAJVER).$(MINVER).$(PATCHLEVEL)" $< > $@

html: $(pod)
	pod2html

htmldist: html
	make_website
