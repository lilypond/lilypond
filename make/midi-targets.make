default:

##local-test: $(outdir)/midi.diff

check: $(outdir)/midi.diff local-WWW-1 local-WWW-2

test: check
