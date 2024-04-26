\version "2.25.16"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "unknown clef type '~a'") "foo")
% We don't need to match the whole list of clefs, the beginning will suffice
#(ly:expect-warning (G_ "supported clefs:\n~a") "  moderntab")

\header {
  texidoc = "Unknown clef name warning displays available clefs"
}

{
  \clef "foo"
  c4
}
