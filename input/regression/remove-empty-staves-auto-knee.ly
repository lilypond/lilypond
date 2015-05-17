\version "2.19.21"

\header {
  texidoc = "
RemoveEmptyStaves should keep the pre-existing value of
@code{auto-knee-gap}.  In this case, the cross-staff beam
should be between the two staves.
"
}

staffMusic = \new StaffGroup {
  <<
    \new Staff = "rh" {
      \relative {
        c'1 \break
        c1 \break
        c8[ c c c
        \change Staff = "lh"
        g, g g g]
        \change Staff = "rh"
	c1
      }
    }
    \new Staff = "lh" {
      \relative {
        \clef bass
        c1
        R1
        R1
	R1
      }
    }
  >>
}

\score {
  \staffMusic
}

\layout {
  ragged-right = ##t
  \context {
    \Staff
    \override Beam.auto-knee-gap = #4.5
    \RemoveEmptyStaves
  }
}

