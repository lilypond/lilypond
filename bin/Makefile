# bin/Makefile

# subdir level:
#
depth = ..
include $(depth)/make/Include.make
# list of distribution files:

SCRIPTS = clearlily cpgento genheader make_patch \
	lily.efence\
	make_version make_website release convert-mudela show-latest \
	conflily
EXTRA_DISTFILES = $(SCRIPTS)
#


EXECUTABLES=convert-mudela 
localinstall: all
	$(INSTALL) -d $(bindir)
	$(INSTALL) -m 755 convert-mudela $(bindir)

localuninstall:
	rm -f $(bindir)/convert-mudela
