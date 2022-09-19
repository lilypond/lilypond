\version "2.23.14"

\header {

  texidoc = "Links to labels and explicit page number (PDF backend only)."

}

#(set-default-paper-size "a6" 'landscape)

\book {
  \label #'front
  \markup { \with-link #'second \concat { "Link to page " \page-ref #'second "0" "?" " with label #'second."  } }
  \markup { \page-link #3 "Explicit link to page 3" }
  \markup { \with-link #'markB "Link to mark B" }

  \pageBreak
  \label #'second

  \score {
    { c'2
      \textMark \markup \with-link #'front { front: \concat { \page-ref #'front "0" "?" ) }}
      c'

      \pageBreak
      \textMark \markup \with-link #'front "B"
      \label #'markB
      d' d'
      }
  }
}
