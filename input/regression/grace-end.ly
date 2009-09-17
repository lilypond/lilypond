\version "2.12.0"

#(ly:set-option 'warning-as-error #f)

\header {
  texidoc="@cindex Grace End
 Grace notes after the last note do not confuse the timing code."
}

\context Voice \relative c' {
  c4 \grace { d16[ d16] }
}
