depth = ../..

.PHONY: snippets

SUBDIRS = new
EXTRA_DIST_FILES = $(call src-wildcard,*.snippet-list) \
  $(call src-wildcard,*.ly) $(call src-wildcard,*.itely) \
  README
SNIPPET_LY_FILES = $(call src-wildcard,*.ly)
OUT_SNIPPET_LY_FILES = $(SNIPPET_LY_FILES:%.ly=out/%.ly)

include $(depth)/make/stepmake.make

default:
