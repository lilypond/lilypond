
\version "2.3.8"
\header {
  texidoc ="Classical song format: one staff with melody and lyrics,
and piano accompaniment."
  
}

melody =  \relative c'' {
  a b c d
}

text = \lyrics {
  Aaa Bee Cee Dee
}

upper = \relative c'' {
  a b c d
}

lower = \relative c {
  a2 c
}

\score {
  <<
      \context Voice = mel {
	  \autoBeamOff
	  \melody
      }
      \lyricsto mel \new Lyrics \text

      \context PianoStaff <<
	  \context Staff = upper \upper
	  \context Staff = lower <<
	      \clef bass
	      \lower
	  >>
      >>
  >>
  \paper {
      \context { \RemoveEmptyStaffContext }
  }  
  \midi { }  
}
