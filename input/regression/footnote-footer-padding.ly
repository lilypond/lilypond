\version "2.16.0"
\header {
  texidoc = "The padding between a footnote and the footer can be tweaked."
}

% TODO: then why isn't it tweaked?

#(set-default-paper-size "a6")

\book {

  \relative c' {
    \footnote
                  \markup { \tiny 1 }
                  #'(1 . -1)
                  \markup { 1. Tiny space below. }
    e1

    \footnote
                  \markup { \tiny 2 }
                  #'(1 . -1)
                  \markup { 2. Tiny space below. }
    e1

    \footnote
                  \markup { \tiny 3 }
                  #'(1 . -1)
                  \markup { 3. Big space below. }
    e1
}}
