\version "2.25.4"

\header {
  texidoc = "The @var{ly:one-line-auto-height-breaking} algorithm puts
  everything on one line (just like @code{ly:one-line-breaking}) and
  sets the page-height to fit the music."
}

\paper { page-breaking = #ly:one-line-auto-height-breaking }

\include "typography-demo.ly"
