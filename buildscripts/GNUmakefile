depth = ..

#STEPMAKE_TEMPLATES=script install install-out po
STEPMAKE_TEMPLATES=script install po

INSTALLATION_FILES=$(outdir)/gettext.py
INSTALLATION_DIR=$(datadir)/python

include $(depth)/make/stepmake.make 

# Should we install these? This should be handled by sysadmin or
# packager but if she forgets...
#INSTALLATION_OUT_SUFFIXES=1
#INSTALLATION_OUT_DIR1=$(datadir)/scripts
#INSTALLATION_OUT_FILES1=$(outdir)/lilypond-login $(outdir)/lilypond-profile

all: $(outdir)/gettext.py

