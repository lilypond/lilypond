# Substitute.make

include $(make-dir)/Substitute.make

sed-endline = sed 's!$$!\\!'
sed-newline = sed 's!$$!\n!'
sed-quotes = sed "s!\'!\\\&!g"
sed-quote-line = sed 's!.*$$!\"&\"!'

date := $(shell date '+%d%b%y'|tr '[a-z]' '[A-Z]' )
DATE = $(date)

# for all FILE in AT_FILES:
# substitute occurrences of @FILE@ with contents $(at-dir)BLA$(at-ext)
sed-atfiles = cat $(foreach i, $(AT_FILES), \
  | sed '/@$i@/r $(at-dir)$i$(at-ext)' | sed 's%@$i@%%g')

# for all VAR in ATVARIABLES
# substitute occurrences of @VAR@ with $(VAR)
sed-atvariables = cat $(foreach i, $(ATVARIABLES), | sed 's!@$i@!$($i)!g')

# these are obsolete
# use ATVARIABLES
sed-date=sed 's!@DATE@!${date}!g'
sed-PACKAGE = sed 's!@PACKAGE@!${PACKAGE}!g'
sed-bash = sed 's!@BASH@!$(BASH)!'
sed-dir-datadir = sed 's!@DIR_DATADIR@!$(DIR_DATADIR)!'
sed-package=sed 's!@package@!${package}!g'
sed-perl = sed 's!@PERL@!$(PERL)!'
sed-python = sed 's!@PYTHON@!$(PYTHON)!'
sed-sh = sed 's!@SH@!$(SH)!'
sed-shell = sed 's!@SH@!$(SHELL)!' | sed 's!@BASH@!$(BASH)!'
sed-version= sed 's!@TOPLEVEL_VERSION@!${TOPLEVEL_VERSION}!g'
sed-step-bindir = sed 's!@STEP_BINDIR@!$(pwd)/$(step-bindir)!'

