\version "2.16.0"

\header {
  texidoc = "By default, we start with page 1, which is on the right hand side
of a double page. In this example, auto-first-page-number is set to ##t.
Although the first measure could go on a page by itself, this would require
stretching the first page badly, so we should automatically set the first page
number to 2 in order to avoid a bad page turn."
}

\paper {
  page-breaking = #ly:page-turn-breaking
  auto-first-page-number = ##t
  print-first-page-number = ##t
}

#(set-default-paper-size "a6")

\layout {
  \context {
    \Staff
    \consists "Page_turn_engraver"
  }
}

\book {
  \score {
    {
      a b c d R1
      \repeat unfold 30 {a4 b c d}
    }
  }
}
