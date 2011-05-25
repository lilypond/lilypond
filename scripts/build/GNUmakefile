depth = ../..

STEPMAKE_TEMPLATES=script install po

EXTRA_DIST_FILES = website-known-missing-files.txt

include $(depth)/make/stepmake.make

# Should we install these? This should be handled by sysadmin or
# packager but if she forgets...
#INSTALLATION_OUT_SUFFIXES=1
#INSTALLATION_OUT_DIR1=$(local_lilypond_datadir)/scripts
#INSTALLATION_OUT_FILES1=$(outdir)/lilypond-login $(outdir)/lilypond-profile

EXTRA_DIST_FILES = website-known-missing-files.txt

all: $(INSTALLATION_FILES)

