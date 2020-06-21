depth = ../..

NAME = documentation

STEPMAKE_TEMPLATES = texinfo

TEXTS = $(call src-wildcard,ANNOUNCE-*[0-9]) \
  $(call src-wildcard,CHANGES-*[0-9]) \
  $(call src-wildcard,ChangeLog*[0-9]) \
  $(call src-wildcard,NEWS-*[0-9]) \
  $(call src-wildcard,THANKS-*[0-9])

include $(depth)/make/stepmake.make

default: local-txt-doc

#urg default local-WWW target uses footify before its time;
# must add footify with txt-to-html target
OUT_TXT_FILES = $(addprefix $(outdir)/, $(addsuffix .txt, $(TEXTS)))
##local-WWW-2: $(OUT_TXT_FILES) $(OUT_HTMLFILES) txt-to-html

$(outdir)/%.txt: %
	$(call ly_progress,Making,$@,(copy))
	cp $< $@

txt-to-html:
	$(buildscript-dir)/text2html $(OUT_TXT_FILES)
	$(footify) $(addprefix $(outdir)/, $(addsuffix .html, $(TEXTS)))
