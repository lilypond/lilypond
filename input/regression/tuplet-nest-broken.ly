
\version "2.19.21"

\header {
    texidoc = "Broken nested tuplets avoid each other correctly.
"
}

\paper {
  ragged-right = ##t
  indent = 0.0
}

\score {
  \new Staff
  <<
    \relative {
      \override Score.Beam.breakable = ##t

      r2

      \tuplet 3/4 {
        \tuplet 3/2 { c''8[ c c] }
        \tuplet 3/2 { c8[ c c] }
        \tuplet 3/2 { c8[ c c] }
      }

      \tuplet 3/4 {
        \tuplet 3/2 { a8[ a a] }
        \tuplet 3/2 { a8[ a a] }
        \tuplet 3/2 { a8[ a a] }
      }

      \override TupletNumber.text = #tuplet-number::calc-fraction-text
      \tuplet 6/4 {
        \tuplet 3/2 {
          a4 a a
        }
        \tuplet 5/3 {
          a4 a a a a
        }
        a4
      }
      r2
    }
    { \repeat unfold 3 { s1 \break } }
  >>
  \layout {
    \context {
      \Voice \remove "Forbid_line_break_engraver"
    }
  }
}
