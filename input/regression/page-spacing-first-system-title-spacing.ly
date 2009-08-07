\version "2.13.2"

#(set-default-paper-size "a6")

\book {

  \header {
    texidoc = "first-system-title-spacing controls the spacing to the title,
provided that it is the first system on a page."
    title = "Title"
  }

  \paper {
    first-system-title-spacing = #'((minimum-distance . 30))
    ragged-bottom = ##t
  }

  { c'1 \pageBreak c'1 }
}