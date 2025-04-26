\version "2.25.26"

\header {
  texidoc = "Auto-melismas are, be default, suppressed if the printed
syllable reaches as far as the start of the last notehead inside the
melisma.  As an exception, auto-melismas are not suppressed in multi-line
melismata."
}

\layout {
  \set Lyrics.autoExtenders = ##t
}

\relative {
  \repeat unfold 11 { c'8( d) }
  c( d \break
  e f g4)
}
\addlyrics {
  l
  ll
  lll
  llll
  lllll
  llllll
  lllllll
  llllllll
  lllllllll
  llllllllll
  lllllllllll
  lllllll
}
