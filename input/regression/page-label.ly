\version "2.12.0"

\header {
  texidoc = "Page labels may be placed inside music or at top-level,
and refered to in markups."
}

#(set-default-paper-size "a6")

#(define-markup-command (toc-line layout props label text) (symbol? markup?)
  (interpret-markup layout props
   (markup #:fill-line (text #:page-ref label "8" "?"))))

\book {
  \markup \huge \fill-line { \null "Title Page" \null }

  \pageBreak

  \label #'toc
  \markup \column {
    \large \fill-line { \null "Table of contents" \null }
    \toc-line #'toc "Table of contents"
    \toc-line #'firstScore "First Score"
    \toc-line #'markA "Mark A"
    \toc-line #'markB "Mark B"
    \toc-line #'markC "Mark C"
    \toc-line #'unknown "Unknown label"
  }

  \pageBreak

  \label #'firstScore
  \score {
    { c'2 c'
      \mark \markup { A (page \concat { \page-ref #'markA "0" "?" ) }} \label #'markA 
      c' c'
      \pageBreak
      \mark "B" \label #'markB
      d' d'
      d' d'
      \once \override Score . RehearsalMark #'break-visibility = #begin-of-line-invisible
      \mark "C" \label #'markC
    }
    \header { piece = "First score" }
  }
}