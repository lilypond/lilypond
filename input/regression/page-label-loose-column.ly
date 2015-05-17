\version "2.19.21"

\header {
  texidoc = "Page labels on loose columns are not ignored: this includes both mid-line
unbreakable columns which only contain labels and columns with empty bar lines (and no other
break-aligned grobs)."
}

#(set-default-paper-size "a6")

\book {
  \markuplist \table-of-contents

  \relative {
    c'2 \tocItem "Mid-line" c^"mid"
    c1
    \bar ""
    \tocItem "Empty bar line"
    c1^"empty"
  }
}
