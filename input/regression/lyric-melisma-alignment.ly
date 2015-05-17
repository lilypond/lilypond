\version "2.19.21"

\header {
  texidoc = "@code{lyricMelismaAlignment} sets the default alignment for melismata.
It works with both automatic and manual melismata."
}

\relative {
  c'4^"auto"( d e f)
  \set melismaBusyProperties = #'()
  c4^"manual" d e f
}
\addlyrics {
  \set lyricMelismaAlignment = #RIGHT
  right-align __
  \set lyricMelismaAlignment = #CENTER
  center  __ _ _ _
}
