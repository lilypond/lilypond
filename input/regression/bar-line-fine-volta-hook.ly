\version "2.23.1"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="A final volta bracket closes at @code{\\fine}."
}

\layout {
  ragged-right = ##t
}

\new Score \fixed c' {
  \repeat volta 2 { } \alternative { f1 { g1 \fine } }
  %% Yes, this A is unreachable in this piece, but it is here to
  %% eliminate the possibility that the volta bracket closes because
  %% of the written end of the music.
  a1
}
