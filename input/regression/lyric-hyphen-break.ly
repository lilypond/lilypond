\version "2.21.0"

\header {

  texidoc = "Hyphens are printed at the beginning of the line only when
they go past the first note, or when property @code{after-line-breaking}
is @code{#t}."

}


\layout {
  indent = 0.0 \cm
  line-width = 3.4 \cm

  \context {
    \Staff \remove "Time_signature_engraver"
  }
}


<<
  \new Voice \relative {
    \time 1/4
    c''16[ c c c]
    c16[ c c c]
    r c16[ c c]
    c16[ c c c]
    c16[ c c c]
  } \addlyrics {
    bla -- bla -- bla -- bla --
    bla -- bla -- bla -- bla --
    \override LyricHyphen.after-line-breaking = ##t
    bla -- bla -- bla --
    bla -- bla -- bla -- bla --
    verylongsyllable -- bla -- bla -- bla
  }
>>
