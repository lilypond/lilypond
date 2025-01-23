\version "2.25.25"

\header {
  texidoc = "Brackets for optional material are placed inside bar lines."
}

#(ly:set-option 'warning-as-error #t)

\fixed c' {
  \partial 4 r4 |
  \startOptionalMaterial g a b c' \stopOptionalMaterial |
}
