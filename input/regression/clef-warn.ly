\version "2.12.0"

#(ly:set-option 'warning-as-error #f)

\header {
  texidoc = "Unknown clef name warning displays available clefs"
}

{
  \clef "foo"
  c4
}
