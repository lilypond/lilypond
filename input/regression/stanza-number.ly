\version "2.19.21"

\header {
  texidoc = "Stanza numbers are put left of their lyric.  They
are aligned in a column."
}

\relative { r4 r4 c''4 c4 }
\addlyrics {
  \skip 2
  \set stanza = "1."
  Foo8 
}
\addlyrics {
  \skip 2
  \set stanza = \markup { 2. }
  FFFooooo8
}
