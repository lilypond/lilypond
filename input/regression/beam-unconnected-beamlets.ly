\header { texidoc = "By setting @code{max-beam-connect}, it is
  possible to create pairs of unconnected beamlets."

  }

\layout {
  ragged-right = ##t
  }
\version "2.11.51"

\relative c'' {
  \override Stem #'max-beam-connect = #1
  c16[ c16]
}
