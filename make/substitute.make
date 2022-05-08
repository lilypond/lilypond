# make/Substitute.make

sed-endline = sed 's!$$!\\!'
sed-newline = sed 's!$$!\n!'
sed-quotes = sed "s!\'!\\\&!g"
sed-quote-line = sed 's!.*$$!\"&\"!'

date := $(shell date '+%d%b%y'|tr '[a-z]' '[A-Z]' )
DATE = $(date)

# for all FILE in AT_FILES:
# substitute occurrences of @FILE@ with contents $(at-dir)BLA$(at-ext)
sed-atfiles = -e '\#' $(foreach i, $(AT_FILES), \
  -e '/@$i@/r $(at-dir)/$i$(at-ext)' -e 's%@$i@%%g')

# for all VAR in ATVARIABLES
# substitute occurrences of @VAR@ with $(VAR)
sed-atvariables = -e '\#' $(foreach i, $(ATVARIABLES), -e 's!@$i@!$($i)!g')

# config_make dep isn't working. Why?
$(outdir)/%: %.in $(config_make) $(depth)/VERSION
	$(call ly_progress,Making,$@,< in)
	rm -f $@
	sed $(sed-atfiles) $(sed-atvariables) < $< > $@

ATVARIABLES = \
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
  local_lilypond_datadir\
  localedir\
  outdir\
  prefix\
  program_prefix\
  program_suffix\
  sharedstatedir\
  src-dir\
  top-src-dir\
