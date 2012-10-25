\version "2.17.6"

\header {
  texidoc = "LilyPond does in-notes.
"
}

#(set-default-paper-size "a6")
\book {
  \relative c' {
    \repeat unfold 5 {
      \once \override FootnoteItem #'footnote = ##f
      \footnote
         "" #'(0 . 0)
         \markup { \box \fill-line { "this is a test" } } NoteHead
      \repeat unfold 5 { a\< b c d\! }
      \footnote #'(-1 . 1) "foobar" NoteHead
      \repeat unfold 5 { a\< b c d\! }
    }
  }
}
