\version "2.23.12"

\header {
  texidoc="A warning is expected when music follows @code{\\fine}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "found music after \\fine"))

\score {
   { c4 \fine d4 }
   \midi {}
}
