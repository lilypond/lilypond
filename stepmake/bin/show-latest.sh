#!/bin/sh
# show-latest.sh --- show latest lily

# @PERL@ @step-bindir@/out/show-latest.pl --package=@package@ $*
# urg
#@step-bindir@/out/show-latest --the-package=@package@ $*
if [ $# -lt 1 ]; then
	print="-p"
fi
@abs-step-bindir@/out/package-latest --the-package=@package@ $* $print

