\header {
  texidoc = "it is allowed to start a score with a page break"
}

\version "2.21.6"


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
