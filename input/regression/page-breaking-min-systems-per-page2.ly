\version "2.25.35"

#(set-default-paper-size "a6")

\header {
  texidoc = "
The min-systems-per-page variable takes precedence over
the desire not to overfill a page.  In this case, systems will
overlap because they are forced to be on the page.
"
}

\book {
  \paper {
    min-systems-per-page = 20
  }

  \*21 c'1
}
