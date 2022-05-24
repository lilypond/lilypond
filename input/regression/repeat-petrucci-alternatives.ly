\version "2.23.8"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "Alternative endings are not expected in ancient music.
Here, the signum repetitionis resembles a modern repeat sign rather
than telling the number of times the alternative is performed."
}

music = \fixed c' {
  c1
  \repeat volta 10 \alternative {
    \volta 1,2,3,4 d1
    \volta 5,6,7 e1
    \volta 8,9 f1
    \volta 10 g1
  }
}

\new ChoirStaff <<
  \new Staff \music
  \new PetrucciStaff \music
>>
