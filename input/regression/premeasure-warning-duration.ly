\version "2.27.2"

\header {
  texidoc = "@code{\\premeasure @var{duration}} produces a warning."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "duration argument reserved for future use"))

{ \premeasure 4 c' }
