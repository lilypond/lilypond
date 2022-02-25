\version "2.17.6"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "tremolo duration is too long"))
#(ly:expect-warning (G_ "tremolo duration is too long"))

\header{
  texidoc="
Stem tremolos or rolls are tremolo signs that look like beam segments
crossing stems.  If the stem is in a beam, the tremolo must be parallel
to the beam.  If the stem is invisible (e.g. on a whole note), the
tremolo must be centered on the note. If the note has a flag (eg. an
unbeamed 8th note), the tremolo should be shortened if the stem is up
and tilted extra if the stem is down.

The tremolos should be positioned a fixed distance from the end of the
stems unless there is no stem, in which case they should be positioned
a fixed distance from the note head.

If an impossible tremolo duration (e.g. :4) is given, a warning is
printed.
"
}

\context Voice \relative c''{
  \textSpannerUp
  \override TextScript.padding = #5
  % The following note should print a warning (quarter tremolo cannot be notated)
  a1:4^":4" a:8^":8" c:16^":16" a:32^":32" a^"x" a:^":"
  a':32 a,,:32
  % The following note should print a warning (quarter tremolo cannot be notated)
  a'4:4 c:8 a:16 c:32 a a: a2:
  \break
  \stemUp
  a4:32 a'4:64 
  \stemDown
  c,4:32 c,4:64
  \stemNeutral
  c'8:16 c c c
  a': a a: a
  c,16:32 c: c c a16:32 a: a a
  c8:16 g: d: a:
  c8:32 f: b: e:
  f8:256[ f,8: f':] f: \noBeam f,,: \noBeam c16:32 \noBeam c'':
}


