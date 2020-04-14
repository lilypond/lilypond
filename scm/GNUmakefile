# scm/GNUmakefile

depth = ..

INSTALLATION_DIR=$(local_lilypond_datadir)/scm
INSTALLATION_FILES=$(SCM_FILES)

XGETTEXT_FLAGS = --language=Scheme
STEPMAKE_TEMPLATES=install po

include $(depth)/make/stepmake.make

SCM_FILES := $(call src-wildcard,*.scm)

TAGS_SOURCES += $(SCM_FILES)
