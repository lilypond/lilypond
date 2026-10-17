\version "2.27.4"

\header {
  texidoc = "@code{\\measureRemainder @var{duration}} produces no warning
but a note."
}

#(ly:set-option 'warning-as-error #t)

{ \measureRemainder 4 c' }
