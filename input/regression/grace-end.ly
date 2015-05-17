\version "2.19.21"

#(ly:set-option 'warning-as-error #f)

\header {
  texidoc="@cindex Grace End
 Grace notes after the last note do not confuse the timing code."
}

\context Voice \relative {
  c'4 \grace { d16 d16 }
}
