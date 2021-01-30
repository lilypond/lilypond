\version "2.16.0"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (_ "no brace found for point size ~S ") 10)
#(ly:expect-warning (_ "defaulting to ~S pt") 10.5)

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
