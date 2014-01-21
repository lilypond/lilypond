\version "2.19.2"

\header {
  texidoc = "Having markup after a non-staff line doesn't confuse
the page layout engine."
}

#(set-default-paper-size "a6")

\book {
  \score {
  <<
     \new Staff <<
       \new Voice = "asdf" { c' d' e' f' }
     >>
     \new Lyrics \lyricsto "asdf" { a b c d }
  >>
  }
  \markup "next song"
  \score {
    <<
      \new Lyrics \lyricmode {la1 la }
      \new Staff \new Voice { a'1 a'1 }
  >>
  }
}

