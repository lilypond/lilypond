\version "1.3.148"
\score {
  \context Staff \notes\relative c''<
    \context Voice=one {
      \voiceOne
      e2->
    }
    \context Voice=two {
      \voiceTwo
      f,2->
    }
  >
}