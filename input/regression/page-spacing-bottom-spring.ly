\version "2.16.0"
#(set-default-paper-size "a6")

\book {
  \header {
    texidoc = "The spring at the bottom of a page is fairly flexible (much more so
than the one at the top), so it does not drag the staff to the bottom of the
page.  However, it is sufficiently stiff to cause stretching."
  }

  \paper {
    ragged-last-bottom = ##f
  }

  \new StaffGroup
  <<
    \new Staff c'1
    \new Staff c'1
  >>
}
