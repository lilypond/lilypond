\version "2.15.2"

\header {
  texidoc = "Stanza numbers are put left of their lyric.  They
are aligned in a column."
}

\relative c'' { r4 r4 c4 c4 }
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
