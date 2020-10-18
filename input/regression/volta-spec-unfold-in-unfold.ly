\version "2.23.0"

\header {
  texidoc="Simultaneous alternatives in nested repeats are unfolded
according to the innermost repeat.  In this test, the upper voice has
two groups of three and the lower voice has three groups of two."
}

\fixed c' <<
  \repeat unfold 2 {
    \repeat unfold 3 {
      f4
      <<
        \volta 1 g-1
        \volta 2 a-2
        \volta 3 b-3
      >>
    }
  }
  \\
  \repeat unfold 3 {
    \repeat unfold 2 {
      c4
      <<
        \volta 1 d-1
        \volta 2 e-2
      >>
    }
  }
>>
