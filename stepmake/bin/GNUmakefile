# bin/Makefile

depth = ..
EXTRA_DIST_FILES = install-sh config.sub config.guess
STEPMAKE_TEMPLATES=script install

include $(depth)/make/stepmake.make 

POST_INSTALL=chmod 755 $(datadir)/bin/*
INSTALLATION_DIR=$(datadir)/bin
INSTALLATION_FILES=$(DIST_FILES)
