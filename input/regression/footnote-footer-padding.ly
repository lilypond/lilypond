\version "2.15.39"
\header {
  texidoc = "The padding between a footnote and the footer can be tweaked."
}

#(set-default-paper-size "a6")

\book {

  \relative c' {
    <>\footnote
                  \markup { \tiny 1 }
                  #'(1 . -1) #'NoteHead
                  \markup { 1. Tiny space below. }
    e1

    <>\footnote
                  \markup { \tiny 2 }
                  #'(1 . -1) #'NoteHead
                  \markup { 2. Tiny space below. }
    e1

    <>\footnote
                  \markup { \tiny 3 }
                  #'(1 . -1) #'NoteHead
                  \markup { 3. Big space below. }
    e1
}}
