\version "2.3.16"

\header {
  texidoc ="Lyrics between two staffs."
}

upper = \relative c'' {
  a b c d
}

lower = \relative c {
  a2 c
}

text = \lyricmode {
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
      \GrandStaff
      \accepts "Lyrics"
    }
    \context {
      %\Lyrics
      \Lyrics
      \consists "Bar_engraver"
    }
  }  
  \midi { }  
}
