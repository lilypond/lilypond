# scm/GNUmakefile

depth = ..

INSTALLATION_DIR=$(local_lilypond_datadir)/scm
INSTALLATION_FILES=$(SCM_FILES)

# This assignment is lazy and uses the function src-wildcard that will only
# be defined when including stepmake. The reason we need this here is the
# definition of local-po in po-targets.make, which checks that ALL_PO_SOURCES
# is not empty.
SCM_FILES = $(call src-wildcard,*.scm)

XGETTEXT_FLAGS = --language=Scheme
STEPMAKE_TEMPLATES=install po

include $(depth)/make/stepmake.make

TAGS_SOURCES += $(SCM_FILES)
