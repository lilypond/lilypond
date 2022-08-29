\version "2.23.13"

\header {
  texidoc = "In MIDI, lyric ties are rendered as Unicode undertie characters
(U+203F)."
}

\score {
  { c'1 1 }
  \addlyrics { a~a a~a~a }
  \midi { }
}
