\version "2.23.12"

\header {
  texidoc = "It is possible for one bookpart to have its independent page numbers
while the others have a common sequence of page numbers."
}

#(set-default-paper-size "a9")

\book {
  \bookpart {
    \paper {
      bookpart-level-page-numbering = ##t
      page-number-type = #'roman-lower
    }
    \markuplist \wordwrap-lines {
      Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean aliquam elementum
tortor, vitae euismod ex malesuada lobortis. Nullam iaculis lorem ante, quis iaculis orci
ultrices vitae. Suspendisse ac lacus eget dolor porttitor elementum vitae ut justo. Duis
in commodo diam.
    }
  }
  \bookpart {
    \repeat unfold 5 { c'1 \pageBreak }
  }
}
