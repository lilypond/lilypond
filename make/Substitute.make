# make/Substitute.make

sed-examples = sed 's!@EXAMPLE_LYS@!${rpmexamples} ${rpmmudocs}!g'
sed-docs=sed 's!@TEXT_DOCS@!${rpmdocs}!g'

ATVARIABLES = \
  BASH\
  DATE\
  date\
  DIR_DATADIR\
  PACKAGE\
  package\
  PERL\
  PYTHON\
  SHELL\
  TOPLEVEL_VERSION\
  step-bindir\
  abs-step-bindir\


