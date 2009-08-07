\version "2.13.2"

#(set-default-paper-size "a6")

\book {

  \header {
    texidoc = "first-system-spacing controls the spacing to the first
non-title staff on every page."
    title = "Title"
  }

  \paper {
    first-system-spacing = #'((minimum-distance . 30))
    ragged-bottom = ##t
  }

  { c'1 \pageBreak c'1 }
}