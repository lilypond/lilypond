ISOLANG = nl
depth = ../..
STEPMAKE_TEMPLATES =  texinfo
LOCALSTEPMAKE_TEMPLATES = lilypond ly doc-i18n-root
# NO_PDF_FILES = 1

include $(depth)/make/stepmake.make

check-xrefs:
	# This is still clumsy, as check_texi_ref.py does not handle
	# an untranslated @ref{Tutorial} (used in an untranslated
	# file) to the translated @node Leerboek
	$(PYTHON) $(auxscript-dir)/check_texi_refs.py --batch \
		$(DOCUMENTS_INCLUDES) -I $(top-src-dir)/Documentation \
		$(auxpython-dir)/manuals_definitions.py
