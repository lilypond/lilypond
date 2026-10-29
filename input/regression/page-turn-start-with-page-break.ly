\header {
  texidoc = "It is allowed to start a score with a page break,
but it gets ignored."
}

\version "2.27.5"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "Ignoring page break at score start"))

#(set-default-paper-size "a6")

\book {
  \header { title = "title" }

  \score {
    {
      \pageBreak
      c'1
    }
  }

}
