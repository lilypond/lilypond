\version "2.25.19"

\header {
  texidoc = "Accidentals can be printed with the @code{\\accidental} markup
command (and derived commands).
In text the @code{\\text-accidental} markup command (and its children) yields
proper aligning.
Both markup commands are protected against overrides of @code{font-name}"
}

\markup \override #'(font-name . "Bitstream Vera Sans, Bold") {
  Accidentals created by the "\\accidental" markup command:
  $(map make-accidental-markup '(-1 -3/4 -1/2 -1/4 0 1/4 1/2 3/4 1))
}

\markup \override #'(font-name . "Bitstream Vera Sans, Bold") {
  Accidentals created by the "\\text-accidental" markup command:
  $(map make-text-accidental-markup '(-1 -1/2 0 1/2 1))
}
