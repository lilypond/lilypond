# bin/Makefile

# subdir level:
#
depth = ..
include $(depth)/make/Include.make
# list of distribution files:

SCRIPTS = clearlily cpgento genheader make_patch \
	lily.efence\
	make-version make-website release convert-mudela show-latest \
	mudela-book conflily
EXTRA_DISTFILES = $(SCRIPTS)
#


EXECUTABLES=convert-mudela mudela-book
localinstall: all
	$(INSTALL) -d $(bindir)
	$(INSTALL) -m 755 convert-mudela $(bindir)
	$(INSTALL) -m 755 mudela-book $(bindir)

localuninstall:
	rm -f $(bindir)/convert-mudela $(bindir)/mudela-book
