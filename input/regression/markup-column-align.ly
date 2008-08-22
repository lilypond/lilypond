\version "2.11.57"
\header {

texidoc = "Fixed horizontal alignment of columns of text can be set using
\left-column, \center-align and \right-column.
"

}

\markup {
  \line {
    \left-column {
      one
      two
      three
    }
    \hspace #4
    \center-align {
      one
      \left-align
      two
      three
    }
    \hspace #6
    \right-column {
      one
      two
      three
    }
  }
}
