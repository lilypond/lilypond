\version "2.23.5"

#(use-modules (ice-9 format))

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (format #f (G_ "no brace found for point size ~,1f ") 10.0))
#(ly:expect-warning (format #f (G_ "defaulting to ~,1f pt") 10.5))

\header {
  texidoc = "If  @code{\\left-brace} or @code{\\right-brace} cannot
find a match for the given point size, it should default
gracefully to either @code{brace0} or @code{brace575} and display
a warning.
"
}

\markup {
  % warning message expected; should default to 10.5 pt for
  % global-staff-size = 20
  \left-brace #10
}
