\version "2.16.0"

#(set-default-paper-size "a6")

\book {

  \header {
    texidoc = "top-system-spacing controls the spacing to the first
non-title staff on every page."
    title = "Title"
  }

  \paper {
    top-system-spacing = #'((minimum-distance . 30))
    ragged-bottom = ##t
  }

  { c'1 \pageBreak c'1 }
}
