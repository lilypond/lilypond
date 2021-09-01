\version "2.23.4"

\header {
  texidoc = "Test markup commands used for conditional constructs.
See also @file{markup-conditionals-single-page.ly}."
}

\header {
  title = "New part"
}

\book {
  \paper {
    #(set-paper-size "a7landscape")
    oddHeaderMarkup = \markup \column {
      \draw-hline
      \fromproperty #'page:page-number-string
      \if \on-first-page "Very first page only"
      \if \on-last-page "Very last page only"
      \if \on-page #5 "Page 5 only"
      \if \on-first-page-of-part "Part first page only"
      \if \on-last-page-of-part "Part last page only"
      \if \single-page "THIS SHOULD NOT BE PRINTED!"
      \if \should-print-page-numbers-global "Everywhere"
      \if \should-print-page-number "Everywhere except on the first page"
      % Test \unless
      \unless \on-first-page "Also everywhere except on the first page"
      \if \should-print-all-headers "THIS SHOULD NOT BE PRINTED!"
    }
    evenHeaderMarkup = \oddHeaderMarkup
  }
  \bookpart {
    { c' }
    \pageBreak
    { c' }
    \pageBreak
    { c' }
  }
  \bookpart {
    { c' }
    \pageBreak
    { c' }
    \pageBreak
    { c' }
  }
}
