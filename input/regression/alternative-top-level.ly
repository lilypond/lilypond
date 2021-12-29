\version "2.23.6"

\header {
  texidoc = "A score with @code{\\alternative} outside of
@code{\\repeat} is processed gracefully.  The visual output is not
important."
}

#(ly:set-option 'warning-as-error #t)

\alternative { \volta 1 s1 }
