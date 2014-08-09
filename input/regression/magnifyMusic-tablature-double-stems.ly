\version "2.19.11"

\header {
  texidoc = "Tablature half-note double-stems should be scaled along
with notation size when using the @code{\magnifyMusic} command."
}

<<
  \new TabVoice = "tabvoice" {
    \tabFullNotation
    \magnifyMusic 0.5 { c4 c2 c4 }
    c4 c2 c4
    \magnifyMusic 2.0 { c4 c2 c4 }
  }
  \new Lyrics \lyricsto "tabvoice" {
    "50%" _ _
    "100%" _ _
    "200%" _ _
  }
>>
