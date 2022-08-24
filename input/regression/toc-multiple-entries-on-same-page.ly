\version "2.22.0"

\header {
  texidoc = "ToC items on the same page stay in the same order as PDF
bookmarks. (The order of ToC items and PDF bookmarks may differ in other
documents.)"
}

#(set-default-paper-size "a6")

\book {
  \markuplist \table-of-contents
  \score {
    {
      \tocItem \markup "1"
      \tocItem \markup "2"
      c'
      \break
      \tocItem \markup "3"
      c'
    }
  }
  \score {
    {
      \tocItem \markup "4"
      \tocItem \markup "5"
      c'
    }
  }
}
