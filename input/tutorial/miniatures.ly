\version "1.7.18"
\header {
  title = "Two miniatures"
  tagline = "Small is beatiful"
}

#(ly:set-point-and-click 'line-column)

\paper { raggedright = ##t}

\score {
    \notes { c'4 d'4 }
    \header {
        opus = "Opus 1."
        piece = "Up" }
}

\score {
    \notes { d'4 c'4 }
    \header {
        opus = "Opus 2."
        piece = "Down" }
}
