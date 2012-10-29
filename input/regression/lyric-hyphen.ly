\version "2.17.6"
\header {
    texidoc = "In lyrics, hyphens may be used."
}
\layout {

    ragged-right= ##t
}

\new Voice { c' (c') c'( c') }
\addlyrics {
  \override Lyrics.LyricSpace.minimum-distance = #5.0
  a -- b x -- y
}






