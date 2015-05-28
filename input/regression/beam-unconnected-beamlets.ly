\header { texidoc = "By setting @code{max-beam-connect}, it is
  possible to create pairs of unconnected beamlets."

  }

\layout {
  ragged-right = ##t
  }
\version "2.19.21"

\relative {
  \override Stem.max-beam-connect = #1
  c''16[ c16]
}
