\version "2.21.2"

\header {
  texidoc = "Page labels on loose columns are not ignored: this includes both mid-line
unbreakable columns which only contain labels and columns with empty bar lines (and no other
break-aligned grobs)."
}

#(set-default-paper-size "a6")

\book {
  \markuplist \table-of-contents

  \relative {
    c'2 \tocItem \markup "Mid-line" c^"mid"
    c1
    \bar ""
    \tocItem \markup "Empty bar line"
    c1^"empty"
  }
}
