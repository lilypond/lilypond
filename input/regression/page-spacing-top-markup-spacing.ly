\version "2.16.0"

#(set-default-paper-size "a6")

\book {

  \header {
    texidoc = "@code{top-markup-spacing} controls the spacing from
the top of the printable area (i.e. the bottom of the top margin)
to a title or markup, when it is the first item on a page."
    title = "Title"
  }

  \paper {
    top-markup-spacing = #'((minimum-distance . 30))
    ragged-bottom = ##t
  }

  { c'1 \pageBreak c'1 }
}
