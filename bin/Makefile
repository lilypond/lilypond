# bin/Makefile

# subdir level:
#
depth = ..
include $(depth)/make/Include.make
# list of distribution files:

SCRIPTS = clearlily cpgento genheader make_patch \
	make_version make_website release convert-mudela
EXTRA_DISTFILES = $(SCRIPTS)
#


localinstall: all
	$(INSTALL) -d $(bindir)
	$(INSTALL) -m 755 convert-mudela $(bindir)

localuninstall:
	rm -f $(bindir)/convert-mudela
