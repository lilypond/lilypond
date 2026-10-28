\version "2.27.5"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "
Grace notes after the last note do not confuse the timing code.
"
}

\context Voice \relative {
  c'4 \grace { d16 d16 }
}
