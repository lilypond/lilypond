\version "2.27.4"

\header {
  texidoc = "@code{\\premeasure @var{duration}} produces no warning but a note."
}

#(ly:set-option 'warning-as-error #t)

{ \premeasure 4 c' }
