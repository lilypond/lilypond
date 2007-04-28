
\header {

  texidoc = "The rehearsal mark is put on top a breakable symbol,
  according to the value of @code{break-align-symbol} value of the
  @code{RehearsalMark}. The same holds for @code{BarNumber} grobs."

}

\version "2.10.0"
  
\relative {
  c1 \mark "foo"
  c1
  \key cis \major
  \clef alto
  \override Score.RehearsalMark #'break-align-symbol = #'key-signature
  \mark "on-key"
  cis
  \key ces \major
  \override Score.RehearsalMark #'break-align-symbol = #'clef
  \clef treble
  \mark "on clef"
  ces
}
