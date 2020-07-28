buildscript-dir = $(top-src-dir)/scripts/build
auxpython-dir = $(src-depth)/python/auxiliar
auxscript-dir = $(src-depth)/scripts/auxiliar
script-dir = $(src-depth)/scripts

export PYTHONPATH:=$(auxpython-dir):$(PYTHONPATH)

# texi2html v5 has fatal errors in the build, so only be strict about
# errors in the version we officially support
ifeq ($(TEXI2HTML_VERSION),1082000)
TEXI2HTML_ERROR_LIMIT=--error-limit=0
endif
