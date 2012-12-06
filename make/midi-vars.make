MIDI_FILES = $(call src-wildcard,*.midi)
HEADER_FILES = $(call src-wildcard,*.header)
OUT_LY_FILES = $(sort ${MIDI_FILES:%.midi=$(outdir)/%.ly} ${LY_FILES:%.ly=$(outdir)/%-midi.ly} )
OUT_FILES = $(OUT_LY_FILES)
HEADER_FIELDS = texidoc options
OUT_DIFF_FILES = ${LY_FILES:%.ly=$(outdir)/%.diff}
MIDI2LY_IGNORE_RES = -I 'Lily was here' -I '^\\version ' -I 'TEXT_EVENT.*GNU LilyPond' -I '^% included from'
DIFF = diff
