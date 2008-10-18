\version "2.11.63"

\header {
  lsrtags = "paper-and-layout"
  texidocs = "@code{\\bookpart} can be used to split a book into several parts.
Each part last page can be affected by @code{ragged-bottom-last}.
"
  doctitle = "Book parts"
}

#(set-default-paper-size "a6")

\book {
  %% book-level paper, which is inherited by all bookparts
  \paper { ragged-last-bottom = ##t }
  %% book-level header, which is inherited by the first bookpart
  \header { title = "Book title" }
  %% first book part
  \bookpart {
    \header { subtitle = "First part" }
    \markup { The first book part }
    \markup \wordwrap { with ragged-last-bottom (see the space below this text) }
  }
  %% an other book part
  \bookpart {
    \header { subtitle = "Second part" }
    { c' }
  }
}
