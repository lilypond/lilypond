\version "2.19.16"

\header {
  texidoc = "Different kinds of silence are not merged into the shared voice even if they begin and end simultaneously."
}

\score { <<
  \new Staff {
    \partcombine
      \relative f' { R1^"R" | s1^"s" | r1^"r" }
      \relative f' { r1_"r" | R1_"R" | s1_"s" }
  }
  \new Staff {
    \partcombine
      \relative f' { r1^"r" | R1^"R" | s1^"s" }
      \relative f' { R1_"R" | s1_"s" | r1_"r" }
  }
>> }
