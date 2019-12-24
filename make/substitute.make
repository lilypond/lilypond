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
  MAJOR_VERSION\
  MINOR_VERSION\
  NCSB_DIR\
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
  prefix\
  program_prefix\
  program_suffix\
  sharedstatedir\
  src-dir\
  top-src-dir\
