\header {
  texidoc = "Newly created contexts can be inserted
anywhere in the vertical alignment. "
}

\version "2.21.0"

\paper {
  ragged-right = ##t
}

\relative <<
  \new Staff = "1" { c'2 c s1 }
  \new Staff = "2" { c2  c s1 }
  \new StaffGroup <<
    \new Staff = "3" { c2  c s1 }
    { \skip 1
    <<
      \lyrics {
        \set alignBelowContext = "1"
        below4 first staff
      }
      \new Staff {
        \set Staff.alignAboveContext = "3"
        \tuplet 6/4 {
          \override TextScript.padding = #3
          c4^"this" d_"staff" e^"above" d_"last" e^"staff" f
        }
      }
    >> }
  >>
>>
