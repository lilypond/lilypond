\version "2.25.16"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (string-join
   (list
    (format #f (G_ "unknown clef type '~a'") "foo")
    (format #f (G_ "supported clefs:\n~a")
     ;; We don't need to match the whole list of clefs; the beginning will
     ;; suffice.
     "  moderntab"))
   "\n"))

\header {
  texidoc = "Unknown clef name warning displays available clefs"
}

{
  \clef "foo"
  c4
}
