depth = ..

STEPMAKE_TEMPLATES=c python-module install po
include $(depth)/make/stepmake.make 

INSTALLATION_FILES=$(OUT_PY_MODULES) $(OUT_PYC_MODULES) $(OUT_SO_MODULES)
INSTALLATION_DIR=$(local_lilypond_datadir)/python

$(outdir)/midi.lo: $(outdir)/config.h
