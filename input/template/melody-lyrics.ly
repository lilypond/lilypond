\version "2.3.4"

\header {
  texidoc = "Melody and lyrics."
}

melody =  \relative c'' {
  a b c d
}

text = \lyrics {
  Aaa Bee Cee Dee
}

\score {
  <<
      \context Voice = one {
	  \set Staff.autoBeaming = ##f
	  \melody
      }
      \lyricsto "one" \new Lyrics \text
  >>
  \paper { }
  \midi  { }
}
