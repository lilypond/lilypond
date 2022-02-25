\version "2.19.21"

#(ly:set-option 'warning-as-error #f)
#(ly:expect-warning (G_ "this is a warning that won't be triggered"))

\header {
  texidoc = "If a warning is expected, but not triggered, print out a
warning about this fact. This will be used to detect missing warnings
in our regtests."
}

\relative { c'4 }
