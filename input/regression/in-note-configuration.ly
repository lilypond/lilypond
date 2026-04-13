\version "2.25.35"

\header {
  texidoc = "In-notes can be controlled via @code{\\paper} variables.
"
}

#(set-default-paper-size "a6")

music = { a4 b8 e c4 d }

\book {
  \relative c'' {
    \override Score.Footnote.footnote = ##f

    \*8 \music
    \footnote #'(1 . 1) "An in-note." NoteHead
    <>-> \*8 \music
    \footnote "" #'(0 . 0) "An in-note without number."
              NoteHead
    <>-> \music
    \footnote "" #'(0 . 0) "Another in-note without number."
              NoteHead
    <>-> \*4 \music
  }

  \paper {
     in-note-system-padding = 10
     in-note-padding = 4
     in-note-direction = #DOWN
  }
}
