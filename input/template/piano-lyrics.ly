\version "2.1.29"

\header {
  texidoc ="Lyrics between two staffs."
}

upper = \notes\relative c'' {
  a b c d
}

lower = \notes\relative c {
  a2 c
}

text = \lyrics {
  Aaa Bee Cee Dee
}

\score {
  \context GrandStaff <<
    \context Staff = upper {
	\context Voice = singer \upper
    }
    \lyricsto "singer" \new Lyrics \text
    \context Staff = lower <<
      \clef bass
      \lower
    >>  
  >>
  \paper {
    \context {
      \GrandStaffContext
      \accepts "Lyrics"
    }
    \context {
      %\LyricsContext
      \LyricsContext
      \consists "Bar_engraver"
    }
  }  
  \midi { }  
}
