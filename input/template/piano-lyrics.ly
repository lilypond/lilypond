\version "2.1.10"

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
    \lyricsto "singer" \new LyricsVoice \text
    \context Staff = lower <<
      \clef bass
      \lower
    >>  
  >>
  \paper {
    \translator {
      \GrandStaffContext
      \accepts "Lyrics"
    }
    \translator {
      %\LyricsVoiceContext
      \LyricsContext
      \consists "Bar_engraver"
    }
  }  
  \midi { }  
}
