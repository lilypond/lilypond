\version "1.5.68"

\score {
  \notes \relative c'' {
    a b c d
    d c b a
  }
  \paper {
    linewidth = -1.
    \translator {
      \StaffContext
      whichBar = #""
      \remove "Time_signature_engraver"
    }
  }
}

