\version "2.19.3"

\header {
  texidoc = "Tuplet numbers are placed next to the beam unless there is
insufficient horizontal space for them, in which case bracket-based
positioning is used and a programming error is issued.

The first tuplet number should be between stems; the second should be
below the noteheads."
}

#(ly:expect-warning (_ "not enough space for tuplet number against beam"))
#(ly:expect-warning (_ "not enough space for tuplet number against beam"))

\layout {
  indent = 0
  ragged-right = ##t
}

\score {
  \relative c' {
    \time 2/4
    \override Beam.auto-knee-gap = 1
    \tuplet 6/4 4 {
      \once \override TupletNumber.text =
        #tuplet-number::calc-fraction-text
      c16 c'' c,, c'' c,, c''
      \once \override TupletNumber.text =
        #(tuplet-number::fraction-with-notes "16" "16")
      c,,16 c'' c,, c'' c,, c''
    }
  }
}
