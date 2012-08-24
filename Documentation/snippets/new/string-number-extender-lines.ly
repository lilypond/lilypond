\version "2.16.0"

\header {
  lsrtags = "editorial-annotations, text, fretted-strings, tweaks-and-overrides, scheme-language"

  texidoc = "
Make an extender line for string number indications, showing that a
series of notes is supposed to be played all on the same string.

"
  doctitle = "String number extender lines"
}

stringNumberSpanner =
#(define-music-function (parser location StringNumber) (string?)
  #{
    \override TextSpanner #'style = #'solid
    \override TextSpanner #'font-size = #-5
    \override TextSpanner #'(bound-details left stencil-align-dir-y) = #CENTER
    \override TextSpanner #'(bound-details left text) = \markup { \circle \number #StringNumber }
  #})


\relative c {
  \clef "treble_8"
  \stringNumberSpanner "5"
  \textSpannerDown
  a8\startTextSpan
  b c d e f\stopTextSpan
  \stringNumberSpanner "4"
  g\startTextSpan a
  bes4 a g2\stopTextSpan
}

