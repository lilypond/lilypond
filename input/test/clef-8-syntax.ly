
\version "2.3.4"
\header {
    texidoc = "@cindex Chord Octavation
Appending @code{_8} or @code{^8} to a clef name will
add an octavation sign to the clef; then the clef name 
is given in quotes (such as \"treble^8\").
" }

\score { 
  \context Voice \relative c {
  \clef "bass_8" c4 c c c |
  \clef "treble^8" c'''4 c c c |
  }
  \paper {
    raggedright = ##t
  }  
  \midi { }
}


