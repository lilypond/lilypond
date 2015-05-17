\version "2.19.21"
\header {
texidoc="LilyPond respects rests, also when there are dynamics"
options=""
}

\score {
  \new Staff \relative {
    \time 2/4 e'4 e r4 e\f |
  }
  \layout {}
  \midi {}
}
