\version "2.1.7"

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
	  \property Staff.autoBeaming = ##f
	  \melody
      }
      \newaddlyrics \new Lyrics \text
  >>
  \paper { }
  \midi  { }
}
