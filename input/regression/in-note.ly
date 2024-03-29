\version "2.23.4"

\header {
  texidoc = "LilyPond does in-notes.
"
}

#(set-default-paper-size "a6")
\book {
  \relative c' {
    \repeat unfold 5 {
      \once \override Score.Footnote.footnote = ##f
      \footnote
         "" #'(0 . 0)
         \markup { \box \fill-line { "this is a test" } } NoteHead
      \repeat unfold 6 { a\< b c d\! } \bar "."
      \footnote #'(-1 . -0.2) "foobar" NoteHead
      \repeat unfold 7 { a\< b c d\! } \bar ".."
    }
  }
}
