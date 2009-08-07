\version "2.13.4"

#(ly:set-option 'warning-as-error #f)

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
