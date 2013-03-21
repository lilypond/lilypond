\version "2.17.15"
#(ly:set-option 'warning-as-error #f)
#(ly:expect-warning (_ "No such instrument: ~a") "bassClar")

\header {
  texidoc = "The @code{switchInstrument} music function prints a warning if
the given instrument definition does not exist."
}

\relative c'
{
  c4
  \instrumentSwitch "bassClar"
  c
}
