# make/Substitute.make

#guh
include $(stepdir)/substitute-vars.make
include $(stepdir)/substitute-rules.make

ATVARIABLES = \
  BASH\
  BUILD_VERSION\
  DATE\
  FONTFORGE\
  GUILE\
  MICRO_VERSION\
  MAJOR_VERSION\
  MINOR_VERSION\
  PACKAGE\
  PATCH_LEVEL\
  PATHSEP\
  PERL\
  PYTHON\
  SHELL\
  TARGET_PYTHON \
  TOPLEVEL_VERSION\
  bindir\
  datadir\
  date\
  lilypond_datadir\
  lilypond_docdir\
  lilypond_libdir\
  local_lilypond_datadir\
  local_lilypond_libdir\
  localedir\
  outdir\
  package\
  prefix\
  program_prefix\
  program_suffix\
  sharedstatedir\
  src-dir\
  top-src-dir\
