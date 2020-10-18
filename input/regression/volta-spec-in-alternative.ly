\version "2.23.0"

\header {
  texidoc="Simultaneous alternatives can appear as elements of
sequential alternatives.  The simultaneous alternatives are used in
order as the sequential alternative is unfolded."
}

music = \fixed c' <<
  \repeat unfold 3 {
    c4
  } \alternative {
    <<
      \volta 1 c'-1
      \volta 2 e'-2
    >>
    b
  }
>>

\new Score {
  \music
}
