\version "2.25.25"

\header {
  texidoc = "Optional-material brackets can be positioned manually.  The start
bracket should encompass note heads from @code{f} to @code{f''} and the end
bracket should encompass note heads from @code{e'} to @code{e'''}."
}

#(ly:set-option 'warning-as-error #t)

\new Staff \with {
  \remove Time_signature_engraver
  \override OptionalMaterialBracket.positions = #'(-5 . 2)
} {
  \startOptionalMaterial
  << <f e'>4 \\ <f'' e'''>4 >>
  \tweak OptionalMaterialBracket.positions #'(-2 . 5)
  \stopOptionalMaterial
}
