\version "2.16.0"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "unknown clef type `~a'") "foo")
% We don't need to match the whole list of clefs, the beginning will suffice
#(ly:expect-warning (G_ "supported clefs: ~a") "C F G G2")

\header {
  texidoc = "Unknown clef name warning displays available clefs"
}

{
  \clef "foo"
  c4
}
