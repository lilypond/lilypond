\version "2.25.35"

\header {
  texidoc = "Beams should subdivided as if there was no tuplet to
consider in this particular case."
}


\paper {
  indent = 0
  ragged-right = ##t
}

\relative c' {
  \time 1/4
  \set subdivideBeams = ##t
  \override TupletNumber.text = #tuplet-number::calc-fraction-text
  \omit Staff.Clef

  c16
  \tuplet 4/1 {
    \tuplet 1/4 {
      \*3 c64
      \tuplet 4/1 {
        \tuplet 1/4 {
          c64 c
        }
      }
      \*3 c64
    }
  }
  c16
  \break

  c16
  \tuplet 4/1 {
    \tuplet 1/4 {
      \*2 c64
      \tuplet 4/1 {
        \tuplet 1/4 {
          \*4 c64
        }
      }
      \*2 c64
    }
  }
  c16
  \break

  c16
  \tuplet 4/1 {
    \tuplet 1/4 {
      c64
      \tuplet 4/1 {
        \tuplet 1/4 {
          \*6 c64
        }
      }
      c64
    }
  }
  c16
  \break
}
