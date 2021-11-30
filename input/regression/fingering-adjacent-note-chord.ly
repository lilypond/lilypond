\version "2.21.0"

\header {
  texidoc = "@code{Fingering} grobs created by the @code{Fingering_engraver}
    (i.e. fingerings entered inside @code{<>}) above/below chords containing
    adjacent notes (seconds) or unison notes should be aligned on the
    main noteheads, i.e., on the noteheads that are on the ``correct'' side of
    the stem.

    Incidentally, this also avoids collisions with accidentals."
}

{
  <>_\markup \teeny \typewriter "Fingering_engraver"
  <e' d'' e''>1^1^4^5
  <a' b' fis''>^1^2^5
  <e' d'' e''>4^1^4^5
  <a' b' fis''>^1^2^5
  <e' d'' e''>_5_4_1
  <eis' e'' fis''>_5_4_1
}
