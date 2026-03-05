\version "2.25.80"

\header {
  texidoc = "Including the deprecated file @file{gregorian.ly} triggers a
warning."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (G_ "gregorian.ly is deprecated; use VaticanaScore context"))

\include "gregorian.ly"

{ s }
