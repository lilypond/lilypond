depth = ../../

# NR 1
SUBDIRS = pitches rhythms expressive repeats simultaneous staff editorial text
# NR 2
SUBDIRS += vocal chords piano percussion guitar strings bagpipes ancient


SUBDIRS += other

### FIXME: temporary template for creating other dirs.
###  will be removed once everything else is stable.
SUBDIRS += null

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc
EXTRA_DIST_FILES=README
include $(depth)/make/stepmake.make

TITLE=LilyPond Examples

