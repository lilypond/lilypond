depth = ..

INSTALLATION_FILES=$(PY_MODULES) $(OUT_PY_MODULES) $(OUT_SO_MODULES)
INSTALLATION_DIR=$(local_lilypond_datadir)/python

STEPMAKE_TEMPLATES=c python-module install po

include $(depth)/make/stepmake.make 

$(outdir)/midi.lo: $(outdir)/config.h
