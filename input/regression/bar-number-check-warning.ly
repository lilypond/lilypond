\version "2.14.0"
#(ly:set-option 'warning-as-error #f)

\header {

texidoc="Bar numbers check may be inserted to check whether the current
bar number is correct.
"

}

\relative c'' {
  c1 | 
  \barNumberCheck #2 % OK
  c1 |
  \barNumberCheck #15 % Warning
  c1
}
