\version "2.19.11"

\header {
  texidoc = "Tablature half-note double-stems should be scaled along
with notation size when using the @code{\magnifyStaff} command."
}

<<
  \new TabVoice = "tabvoice" {
    \tabFullNotation
    \magnifyStaff 0.5 c4 c2 c4
    \magnifyStaff 1.0 c4 c2 c4
    \magnifyStaff 2.0 c4 c2 c4
  }
  \new Lyrics \lyricsto "tabvoice" {
    "50%" _ _
    "100%" _ _
    "200%" _ _
  }
>>
