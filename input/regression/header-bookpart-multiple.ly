\version "2.16.0"

\header {
  texidoc="
A second bookpart-level header block shall retain previously set values from a first header block at the same or higher levels unless overriden.
"
}

\header { composer = "Composer correct (set at top level)" }
\book {
  \header {
    title = "Title correct (set in book)"
    subtitle = "Subtitle incorrect (to be superseded in bookpart)"
  }
  \bookpart {
    \header {
      subtitle = "Subtitle correct (superseded in bookpart)"
      piece = "Piece incorrect (to be superseded at bookpart level)"
    }
    \header {
      piece = "Piece correct (superseded at bookpart level)"
    }
    \markup \vspace #2
    \markup { \bold Note: expect title, subtitle, piece and composer. }
    \markup \vspace #2
    \score {
      \new Staff { c'4 }
    }
  }
}
