\version "2.16.0"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "unknown translator: `%s'") "Rhythmic_column_engraver_foo")
#(ly:expect-warning (ly:translate-cpp-warning-scheme "cannot find: `%s'") "Rhythmic_column_engraver_foo")

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
