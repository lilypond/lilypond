\version "2.16.0"

\header {
  texidoc = "@code{lyricMelismaAlignment} sets the default alignment for melismata.
It works with both automatic and manual melismata."
}

\relative c' {
  c4^"auto"( d e f)
  \set melismaBusyProperties = #'()
  c4^"manual" d e f
}
\addlyrics {
  \set lyricMelismaAlignment = #RIGHT
  right-align __
  \set lyricMelismaAlignment = #CENTER
  center  __ _ _ _
}
