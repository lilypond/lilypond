\version "1.5.68"

\header {
  texidoc ="Classical song format: one staff with melody and lyrics, and piano accompaniment."
  
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
  <
    \addlyrics
      \context Staff = mel {
        \property Staff.autoBeaming = ##f
        \property Staff.automaticMelismata = ##t
        \melody
      }
      \context Lyrics \text

    \context PianoStaff <
      \context Staff = upper \upper
      \context Staff = lower <
	\clef bass
	\lower
      >
    >
  >
  \paper {
    \translator { \HaraKiriStaffContext }
  }  
  \midi { }  
}
