# make/Substitute.make

#guh
include $(stepdir)/substitute-vars.make
include $(stepdir)/substitute-rules.make

ATVARIABLES = \
  BASH\
  DATE\
  sharedstatedir\
  GUILE\
  date\
  datadir\
  lilypond_datadir\
  lilypond_libdir\
  local_lilypond_datadir\
  local_lilypond_libdir\
  localedir\
  PACKAGE\
  package\
  PATHSEP\
  PERL\
  prefix\
  program_prefix\
  program_suffix\
  PYTHON\
  SHELL\
  TOPLEVEL_VERSION\
  step-bindir\

