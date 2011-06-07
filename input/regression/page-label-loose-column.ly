\version "2.14.0"

\header {
  texidoc = "Page labels on loose columns are not ignored: this includes both mid-line
unbreakable columns which only contain labels and columns with empty bar lines (and no other
break-aligned grobs)."
}

#(set-default-paper-size "a6")

\book {
  \markuplines \table-of-contents

  \relative c' {
    c2 \tocItem "Mid-line" c^"mid"
    c1
    \bar ""
    \tocItem "Empty bar line"
    c1^"empty"
  }
}
