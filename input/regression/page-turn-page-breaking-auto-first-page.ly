\version "2.11.51"

\header {
  texidoc = "By default, we start with page 1, which is on the right hand side
of a double page. In this example, auto-first-page-number is set to ##t and the
music won't fit on a single page, so we should automatically set the first page
number to 2 in order to avoid a bad page turn."
}

\paper {
  page-breaking = #ly:page-turn-breaking
  auto-first-page-number = ##t
  print-first-page-number = ##t
}

#(set-default-paper-size "a6")

\book {
  \score {
    {\repeat unfold 40 {a b c d}}
  }
}