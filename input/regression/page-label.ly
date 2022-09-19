\version "2.23.14"

\header {
  texidoc = "Page labels may be placed inside music or at top-level,
and referred to in markups.  Labels created with @code{\\tocItem}
(and thus bearing an internally-generated unique identifying symbol)
remain referrable by their user-specified name."
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
      \textMark \markup {
        A (page \concat { \page-ref #'markA "0" "?" ) }
      } \label #'markA
      c' c'
      \pageBreak
      \textMark "B" \label #'markB
      d' d'
      \tocItem parent "This shouldn’t be printed"
      d' d'
      \textEndMark "C" \label #'markC
      \tocItem parent.markC "This shouldn’t be printed"
    }
    \header { piece = "First score" }
  }
}
