# bin/Makefile

depth = ..
include $(depth)/make/Include.make

# list of distribution files:
SCRIPTS = clearlily cpgento make-patch lily.efence \
	make-version release conflily
PERL_SCRIPTS_IN = $(wildcard *.in)
PERL_SCRIPTS = $(addprefix $(outdir)/, $(PERL_SCRIPTS_IN:.in=))
EXTRA_DISTFILES = $(SCRIPTS) $(PERL_SCRIPTS_IN)
#

all: $(PERL_SCRIPTS)

$(outdir)/%: %.in
#	cd .. && CONFIG_FILES=bin/$(notdir $@) CONFIG_HEADERS= ./config.status
#	mv $(< :.in=) $@
# this nukes make/out/Configure_variables.make.
	sed 's!@PERL@!$(PERL)!' < $< > $@
	chmod 755 $@


EXECUTABLES=convert-mudela mudela-book
localinstall: all
	$(INSTALL) -d $(bindir)
	$(INSTALL) -m 755 $(outdir)/convert-mudela $(bindir)
	$(INSTALL) -m 755 $(outdir)/mudela-book $(bindir)

localuninstall:
	rm -f $(bindir)/convert-mudela $(bindir)/mudela-book
