\version "2.13.4"

\header {
    texidoc = "A loose line (eg. a lyric line) at the bottom of a system
gets spaced appropriately."
}

\layout {
  ragged-right = ##t
  \context {
    \Lyrics
    \override VerticalAxisGroup #'inter-loose-line-spacing #'space = #20
  }
}
<<
    \new Staff \relative {
	d'2 d c4 bes a2 \break
    }
    \addlyrics {
	My first Li -- ly song,
    }
    \addlyrics {
	Not much can go wrong!
    }
>>

