\version "2.2.0"

\header {
  texidoc = "Melody and lyrics."
}

melody = \notes \relative c'' {
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
