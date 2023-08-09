\version "2.25.8"

\header {
  doctitle = "Beam subdivision of duplets/powers of@tie{}2."

  texidoc = "When subdividing tuplet ratios whose numerators are powers
of@tie{}2, the beamlet removal depth of each subdivision should vary by
the same level different, dependending on the tuplet ratio magnitude"
}


\paper {
  indent = 0
  ragged-right = ##t
}

\relative c' {
  \time 1/4
  \set subdivideBeams = ##t
  \omit Staff.Clef
  \override TupletNumber.text = #tuplet-number::calc-fraction-text

  \tuplet 1/2 {
    \repeat unfold 8 c64
  }
  \break

  \tuplet 1/4 {
    \repeat unfold 8 c128
  }
  \break

  \tuplet 1/8 {
    \repeat unfold 8 c256
  }
  \break

  \time 1/16
  \tuplet 2/1 {
    \repeat unfold 8 c64
  }
  \break

  \tuplet 4/1 {
    \repeat unfold 16 c64
  }
  \break

  \tuplet 8/1 {
    \repeat unfold 32 c64
  }
  \break
}
