\version "2.13.1"

\header {
  texidoc = "When a custom key signature has entries which are
limited to a particular octave, such alterations should persist
indefinitely or until a new key signature is set.

Here, only the fis' shows an accidental, since it is outside the
octave defined in @code{keySignature}.
"
}

\relative c' {
  \set Staff.keySignature = #`(((0 . 3) . ,SHARP)
                               ((0 . 5) . ,FLAT)
                               ((0 . 6) . ,FLAT))
  fis fis as bes
  fis' as, as bes
}
