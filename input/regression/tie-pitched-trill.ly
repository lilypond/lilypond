\version "2.14.2"

\header {
  texidoc = "The pitch of a pitched trill should not trigger a warning for
  unterminated ties."
}

\relative c' {
  \pitchedTrill
  c1~\startTrillSpan d
  c1\stopTrillSpan
}
