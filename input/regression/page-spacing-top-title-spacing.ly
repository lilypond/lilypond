\version "2.13.36"

#(set-default-paper-size "a6")

\book {

  \header {
    texidoc = "@var{top-markup-spacing} controls the spacing
from the top margin to a title (or markup), provided that it is
the first system on a page."
    title = "Title" }

  \paper {
    top-markup-spacing = #'((minimum-distance . 30))
    ragged-bottom = ##t
  }

  { c'1 \pageBreak c'1 }
}