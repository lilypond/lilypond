\version "2.16.0"

\header {
  texidoc = "A @code{\\book} or @code{\\bookpart} identifier can contain
top-level markup and page-markers."
}

mypart = \bookpart {
  \relative c' { c1 }
  \label #'marker
  \markup { Page \page-ref #'marker "8" "?" }
}

\bookpart { \mypart }
