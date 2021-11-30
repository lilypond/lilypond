\version "2.21.0"

\header {
  texidoc = "@code{Fingering} grobs created by the
    @code{New_fingering_engraver} (i.e. fingerings entered outside @code{<>})
    with @code{fingeringOrientations} set to @code{up} or @code{down} avoid
    accidentals of displaced notes that might get into the way in chords
    containing adjacent notes (seconds) or unison notes.

    With @code{\override Fingering.X-align-on-main-noteheads = ##t},
    the fingerings oriented @code{up} and @code{down} will be arranged
    in a straight column aligned on the noteheads on the ``correct'' side
    of the stem."
}

{
  <>_\markup \teeny \typewriter "New_fingering_engraver"
  \set fingeringOrientations = #'(up)
  <e'-1 d''-4 e''-5>1
  <a'-1 b'-2 fis''-5>
  <e'-1 d''-4 e''-5>4
  <a'-1 b'-2 fis''-5>
  \set fingeringOrientations = #'(down)
  <e'-1 d''-4 e''-5>
  <eis'-1 e''-4 fis''-5>
  \bar "."
  <>^\markup \teeny \typewriter "X-align-on-main-noteheads = ##t"
    _\markup \teeny \typewriter "New_fingering_engraver"
  \override Fingering.X-align-on-main-noteheads = ##t
  \set fingeringOrientations = #'(up)
  <e'-1 d''-4 e''-5>1
  <a'-1 b'-2 fis''-5>
  <e'-1 d''-4 e''-5>4
  <a'-1 b'-2 fis''-5>
  \set fingeringOrientations = #'(down)
  <e'-1 d''-4 e''-5>
  <eis'-1 e''-4 fis''-5>
}
