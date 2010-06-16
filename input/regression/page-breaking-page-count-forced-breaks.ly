\version "2.13.23"

#(set-default-paper-size "a6")

\book {
  \header {
    texidoc = "When specifying page-count together with manual \pageBreaks,
page-count must be a list with one more element than the number of
\pageBreaks and each element refers to the number of pages between the
appropriate consecutive \pageBreaks."
  }

  \paper {
    page-count = #'(1 2)
  }

  { c'1 c'1 \pageBreak c'1 c'1 c'1}
}