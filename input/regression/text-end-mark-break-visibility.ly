\version "2.23.14"

\header {
  texidoc = "A text mark created by @code{\\textEndMark} is visible everywhere
except at the beginning of a line."
}

#(set-default-paper-size "a7")

\fixed c' {
  \repeat volta 2 { c1 }
  \textEndMark \markup \italic "ad lib."
  \repeat volta 2 { c1 }
  \textEndMark \markup \italic "ad lib."
  \break
  c'1
  \tweak direction #DOWN \tweak font-size -1.5 \textEndMark "Composed Sept. 10, 2022"
}
