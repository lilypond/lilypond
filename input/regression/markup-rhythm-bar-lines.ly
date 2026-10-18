\version "2.27.5"

\header {
  texidoc = "@code{\\markup \\rhythm} prints bar lines as if the note heads
were in the bottom space of a four-space staff."
}

testMusic = {
  \bar "[|:"
  8 \bar "'"
  8 \bar ","
  8 \bar "|"
  8 \bar "!"
  8 \bar ";"
  8 \bar ":|."
  %% \bar "k" is omitted because of incorrect vertical placement.
  %% \bar "S" is omitted because of incorrect vertical placement.
}

testMarkup = \markup {
  Lo! \rhythm {
    \testMusic
  }
}

\new StandaloneRhythmStaff \with {
  instrumentName = "not markup"
} {
  \testMusic
}

\markup \fontsize #-7 \testMarkup
\markup \testMarkup
\markup \fontsize #7 \testMarkup
