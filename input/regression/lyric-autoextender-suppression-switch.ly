\version "2.25.26"

\header {
  texidoc = "The removal of short auto-melismas may be switched off."
}

\layout {
  \set Lyrics.autoExtenders = ##t
}

\relative {
  \repeat unfold 4 { c'8( d e d) }
}
\addlyrics {
  Supercalifragilisticexpialidocious
  Supercalifragilisticexpialidocious
  \override LyricExtender.remove-short-autoextender = ##f
  Supercalifragilisticexpialidocious
  Supercalifragilisticexpialidocious
}
