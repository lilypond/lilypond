# -*-Makefile-*-
# title	   Yolily_Toplevel_targets.make

local-dist: do-top-doc

$(package-icon):
	$(MAKE) -C Documentation/pictures icon

do-top-doc:
	-$(MAKE) -C Documentation/topdocs/ README_TOP_FILES="$(README_TXT_FILES)" copy-to-top

$(README_TXT_FILES): do-top-doc

localclean:


# if you fix this, please fix yodl too!
check-top-web:
	$(MAKE) -C Documentation/topdocs WWW

