# make/Substitute.make

#guh
include $(stepdir)/substitute-vars.make
include $(stepdir)/substitute-rules.make

ATVARIABLES = \
  BASH\
  DATE\
  GUILE\
  date\
  datadir\
  PACKAGE\
  package\
  PATHSEP\
  PERL\
  program_prefix\
  program_suffix\
  PYTHON\
  SHELL\
  TOPLEVEL_VERSION\
  step-bindir\

