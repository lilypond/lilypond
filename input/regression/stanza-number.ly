\version "2.7.13"

\header { texidoc = "Stanza numbers are put left of their lyric.  They
are aligned in a column."  }

\layout { raggedright = ##t }

\relative c'' { r4 r4 c4 c4 }
\addlyrics {
  \skip 2
  \set stanza = "1."
  Foo8 
}
\addlyrics {
  \skip 2
  \set stanza = "2."
  FFFooooo8
}



