
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


