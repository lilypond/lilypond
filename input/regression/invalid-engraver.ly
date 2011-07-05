\version "2.14.0"

#(ly:set-option 'warning-as-error #f)

\header {
  texidoc = "Engravers which do not exist produce a warning."
}

\layout {
  \context {
    \Voice
    \consists "Rhythmic_column_engraver_foo"
  }
}

{ a4 }
