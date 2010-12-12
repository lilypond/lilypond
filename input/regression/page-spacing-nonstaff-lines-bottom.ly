\version "2.13.42"

\header {
  texidoc = "A non-staff line (such as @code{Lyrics}) at the
bottom of a system gets spaced appropriately."
}

\layout {
  ragged-right = ##t
  \context {
    \Lyrics
    \override VerticalAxisGroup #'nonstaff-nonstaff-spacing #'basic-distance = #20
  }
}
<<
    \new Staff \relative c'' {
	d2 d c4 bes a2 \break
    }
    \addlyrics {
	My first Li -- ly song,
    }
    \addlyrics {
	Not much can go wrong!
    }
>>

