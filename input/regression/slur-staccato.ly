
\version "2.1.22"
\header {
texidoc="Manual hack for slur and staccato."
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
