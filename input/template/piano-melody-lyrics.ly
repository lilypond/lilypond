
\version "2.3.2"
\header {
  texidoc ="Classical song format: one staff with melody and lyrics,
and piano accompaniment."
  
}

melody = \notes \relative c'' {
  a b c d
}

text = \lyrics {
  Aaa Bee Cee Dee
}

upper = \notes\relative c'' {
  a b c d
}

lower = \notes\relative c {
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
