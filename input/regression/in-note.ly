\version "2.15.39"

\header {
  texidoc = "LilyPond does in-notes.
"
}

#(set-default-paper-size "a6")
\book {
  \relative c' {
    \repeat unfold 5 {
      \once \override FootnoteItem #'footnote = ##f
      <>\footnote
                    "" #'(0 . 0) #'NoteHead \markup { \box \fill-line { "this is a test" } }
      \repeat unfold 5 { a\< b c d\! }
      <>\footnote #'(-1 . 1) #'NoteHead "foobar"
      \repeat unfold 5 { a\< b c d\! }
    }
  }
}
