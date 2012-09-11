\version "2.16.0"
\header {

texidoc = "Fixed horizontal alignment of columns of text can be set using
\left-column, \center-column and \right-column.
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
    \center-column {
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
