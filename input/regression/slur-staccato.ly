
\version "2.1.30"
\header {
texidoc="An extra offset may be added between a slur and staccato(s)."
}

\paper { raggedright = ##t}

\score {
  \context Staff \notes\relative c'' {
    \override Slur 
      #'attachment-offset = #'((0 . 1) . (0 . 1))
    a-.( g-.  a)-.
    \override Slur 
      #'attachment-offset = #'((0 . 1.5) . (0 . 1.5))
    b-.( a-.  b)-.
  }
}	
