
\header {

  texidoc = "The rehearsal mark is put on top a breakable symbol,
  according to the value of @code{rehearsalMarkAlignSymbol}."

}

\version "2.7.19"
  
\relative {
  c1 \mark "foo"
  \key cis \major
  \clef alto
  \set Score.rehearsalMarkAlignSymbol = #'key-signature
  \mark "on-key"
  cis
 \key ces \major
  \set Score.rehearsalMarkAlignSymbol = #'clef
  \clef treble
  \mark "on clef"
  ces
}
