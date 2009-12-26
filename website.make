################################################################
# website (without the rest of the docs)

#top-src-dir=$(HOME)/src/lilypond/
EXTRACT_TEXI_FILENAMES=scripts/build/out/extract_texi_filenames
CREATE_VERSION=$(top-src-dir)/scripts/build/create-version-itexi.py
out=out-website
EXAMPLES=$(HOME)/media/examples/
PICTURES=$(HOME)/media/pictures


# don't include web
MANUALS=$(wildcard $(top-src-dir)/Documentation/*.tely)
MANUALS+=$(top-src-dir)/Documentation/contributor.texi


website-version:
	mkdir -p $(out)
	python $(CREATE_VERSION) > $(out)/version.itexi

website-xrefs: website-version
	$(EXTRACT_TEXI_FILENAMES) -I $(top-src-dir)/Documentation/ \
		-I $(out) -o $(out) --split=node \
		$(top-src-dir)/Documentation/web.texi
	$(foreach manual, $(MANUALS), \
		$(EXTRACT_TEXI_FILENAMES) -I $(top-src-dir)/Documentation/ \
		-I $(out) -o $(out) $(manual) && ) :

website-texinfo: website-version website-xrefs
	SRC_DIR=$(top-src-dir)/Documentation/ \
		PERL_UNICODE=SD texi2html --prefix=index \
		--split=section \
		--I=$(top-src-dir)/Documentation/ \
		--I=$(out) \
		--init-file=/home/gperciva/src/lilypond/Documentation/lilypond-texi2html.init \
		-D web_version \
		--output=$(out)/website/ \
		$(top-src-dir)/Documentation/web.texi

website-css:
	cp $(top-src-dir)/Documentation/css/*.css $(out)/website/

website-pictures:
	cp -r $(PICTURES) $(out)/website/
	ln -sf website/pictures $(out)/pictures

website-examples:
	cp -r $(EXAMPLES) $(out)/website/


website: website-texinfo website-css website-pictures website-examples


