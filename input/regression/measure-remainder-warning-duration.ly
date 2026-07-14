\version "2.27.2"

\header {
  texidoc = "@code{\\measureRemainder @var{duration}} produces a warning."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "duration reserved for future use"))

{ \measureRemainder 4 c' }
