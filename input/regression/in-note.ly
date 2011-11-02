\version "2.15.17"

\header {
  texidoc = "LilyPond does in-notes.
"
}

#(set-default-paper-size "a6")
\book {
  \relative c' {
    \repeat unfold 5 {
      \once \override FootnoteItem #'footnote = ##f
      \footnoteGrob #'NoteHead #'(0 . 0)
                    "" \markup { \box \fill-line { "this is a test" } }
      \repeat unfold 5 { a\< b c d\! }
      \autoFootnoteGrob #'NoteHead #'(-1 . 1) "foobar"
      \repeat unfold 5 { a\< b c d\! }
    }
  }
}
