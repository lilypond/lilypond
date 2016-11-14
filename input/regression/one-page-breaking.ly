\version "2.19.36"

\header {
  texidoc = "The @var{ly:one-page-breaking} algorithm puts everything
on one page by adjusting the page-height to fit the content.
"
}

\paper {
  paper-width = 120
  page-breaking = #ly:one-page-breaking
}

\include "typography-demo.ly"
