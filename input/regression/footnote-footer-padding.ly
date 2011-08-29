\version "2.14.0"
\header {
  texidoc = "The padding between a footnote and the footer can be tweaked."
}

#(set-default-paper-size "a6")

\book {

  \relative c' {
    \footnoteGrob #'NoteHead
                  #'(1 . -1)
                  \markup { \tiny 1 }
                  \markup { 1. Tiny space below. }
    e1

    \footnoteGrob #'NoteHead
                  #'(1 . -1)
                  \markup { \tiny 2 }
                  \markup { 2. Tiny space below. }
    e1

    \footnoteGrob #'NoteHead
                  #'(1 . -1)
                  \markup { \tiny 3 }
                  \markup { 3. Big space below. }
    e1
}}
