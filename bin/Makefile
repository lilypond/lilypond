# bin/Makefile

depth = ..
include $(depth)/make/Include.make

# list of distribution files:
PERL_SCRIPTS_IN = $(wildcard *.pl)
PERL_SCRIPTS = $(addprefix $(outdir)/, $(PERL_SCRIPTS_IN:.pl=))
SH_SCRIPTS_IN = $(wildcard *.sh)
SH_SCRIPTS = $(addprefix $(outdir)/, $(SH_SCRIPTS_IN:.sh=))
PYTHON_SCRIPTS_IN = $(wildcard *.py)
PYTHON_SCRIPTS = $(addprefix $(outdir)/, $(PYTHON_SCRIPTS_IN:.py=))
EXTRA_DISTFILES = $(SH_SCRIPTS_IN) $(PERL_SCRIPTS_IN) $(PYTHON_SCRIPTS_IN) 


all: $(PERL_SCRIPTS) $(PYTHON_SCRIPTS) $(SH_SCRIPTS)

$(outdir)/%: %.pl
	sed 's!@PERL@!$(PERL)!' < $< > $@
	chmod 755 $@

#FIXME.  Check for bash?
$(outdir)/%: %.sh
	sed 's!@SH@!$(SHELL)!' < $< > $@
	chmod 755 $@

$(outdir)/%: %.py
	sed 's!@PYTHON@!$(PYTHON)!' < $< > $@
	chmod 755 $@

EXECUTABLES=convert-mudela mudela-book ly2dvi

localinstall: all
	$(INSTALL) -d $(bindir)
	cd $(outdir); \
		for a in $(EXECUTABLES); \
		do $(INSTALL) -m 755 $$a $(bindir); \
		done

localuninstall:
	for a in $(EXECUTABLES); \
		do rm -f $(bindir)/convert-mudela $(bindir)/mudela-book; \
		done
