\version "1.5.68"

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
  \context GrandStaff <
    \addlyrics
      \context Staff = upper \upper
      \context Lyrics \text
    \context Staff = lower <
      \clef bass
      \lower
    >  
  >
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
