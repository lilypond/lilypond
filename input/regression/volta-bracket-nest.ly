\version "2.24.2"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="When alternatives are nested, volta brackets stack with the
outermost alternative on top.  In this case, alternatives for volte
2-5 and 6 are nested inside an alternative for volte 2-6."
}

\fixed c' {
  \repeat volta 6 {
    d1
    \alternative {
      \volta 1 e1
      \volta 2,3,4,5,6 {
        f1
        \alternative {
          \volta 2,3,4,5 g1
          \volta 6 a1
        }
      }
    }
  }
  b1
}
