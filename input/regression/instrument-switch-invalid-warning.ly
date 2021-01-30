\version "2.19.21"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (_ "No such instrument: ~a") "bassClar")

\header {
  texidoc = "The @code{switchInstrument} music function prints a warning if
the given instrument definition does not exist."
}

\relative
{
  c'4
  \instrumentSwitch "bassClar"
  c
}
