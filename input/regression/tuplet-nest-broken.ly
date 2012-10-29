
\version "2.17.6"

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
    \relative c'' {
      \override Score.Beam.breakable = ##t

      r2

      \times 4/3 {
        \times 2/3 { c8[ c c] }
        \times 2/3 { c8[ c c] }
        \times 2/3 { c8[ c c] }
      }

      \times 4/3 {
        \times 2/3 { a8[ a a] }
        \times 2/3 { a8[ a a] }
        \times 2/3 { a8[ a a] }
      }

      \override TupletNumber.text = #tuplet-number::calc-fraction-text
      \times 4/6 {
        \times 2/3 {
          a4 a a
        }
        \times 3/5 {
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
