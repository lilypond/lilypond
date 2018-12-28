\version "2.21.0"

\header {
  texidoc = "Different kinds of silence are not merged into the shared voice even if they begin and end simultaneously; however, when rests and skips are present in the same part, the skips are ignored."
}

\score { <<
  \new Staff {
    \partCombine
      \relative f' { R1^"R" | s1^"s" | r1^"r" | << R1 s1 s4 >> | << r1 s2 s4 >> }
      \relative f' { r1_"r" | R1_"R" | s1_"s" | << s4 s1 R1 >> | << s4 s2 r1 >> }
  }
  \new Staff {
    \partCombine
      \relative f' { r1^"r" | R1^"R" | s1^"s" | << s4 s1 R1 >> | << s4 s2 r1 >> }
      \relative f' { R1_"R" | s1_"s" | r1_"r" | << R1 s1 s4 >> | << r1 s2 s4 >> }
  }
>> }
