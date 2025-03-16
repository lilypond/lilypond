depth = ../../..

TEMPLATES = lilypond ly lysdoc 

MUSICXML_FILES = $(call src-wildcard,*.xml)
MUSICMXL_FILES = $(call src-wildcard,*.mxl)  # Allow .mxl for compressed files

COLLATED_FILES = $(sort $(MUSICXML_FILES) $(MUSICMXL_FILES) $(ITEXI_FILES))

LYS_TO_TELY_OPTIONS = "--fragment-options=htmlprintfilename,texidoc"

.SUFFIXES: .xml .mxl


include $(depth)/make/lilypond.make

$(outdir)/%.ly:  %.xml
	$(call ly_progress,Making,$@,< xml)
	$(PYTHON) $(MUSICXML2LY) -o $@ $<

$(outdir)/%.ly:  %.mxl
	$(call ly_progress,Making,$@,< mxl)
	$(PYTHON) $(MUSICXML2LY) -z -o $@ $<

TITLE=Unofficial MusicXML test suite
AUTHOR=Reinhold Kainhofer
