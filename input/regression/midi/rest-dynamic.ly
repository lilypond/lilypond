\header {
texidoc="LilyPond respects rests, also when there are dynamics"
options=""
}

\score {
  \new Staff \relative c' {
    \time 2/4 e4 e r4 e\f |
  }
  \layout {}
  \midi {}
}
