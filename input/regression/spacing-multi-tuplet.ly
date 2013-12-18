\version "2.17.11"

 \header{

  texidoc = "Concurrent tuplets should be equidistant on all staves."
}

\paper {ragged-right = ##f }

\relative c' { 
  \context StaffGroup << 
    \new Staff  \context Voice { 
      \tuplet 10/8 {  c8[ c c c c c c c c c] }
    }
    \new Staff  \context Voice { 
      \tuplet 8/8 {  c8[ c c c c c c c] }
    }
  >>
}



