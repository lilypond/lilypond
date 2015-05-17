\version "2.19.21"

\header {
  texidoc="
Changing the header fields in a book or a bookpart shall not have any effect on the global default values.
"
}

\markup \vspace #2
\markup { \bold Note: expect only title. }
\markup \vspace #2

\header {
  title = "Title correct (set at top level)"
}
\score {
  \relative { c'1 }
}


\book {
  % This should NOT set a global subtitle for the first score above:
  \header {
    subtitle = "Subtitle (set at book level)"
  }
  \markup \vspace #2
  \markup { \bold Note: expect title and subtitle. }
  \markup \vspace #2
  %% Do we have a title, and is the subtitle set?
  \score {
    \new Staff { c'1 }
  }
}
