\version "2.19.21"

\header {

    texidoc= "A LyricExtender should end at the right place even if there are more notes in the voice than lyrics."

}

<<

\new Staff \relative {
  \new Voice = "upper" {
    \voiceTwo
    g'1( |
    c,) |
    d |
  }
}
\new Lyrics \lyricsto "upper" { Ah __ }

>>
