\version "2.23.4"

\header {
  texidoc = "A score with lyrics and no music fails gracefully."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "cannot find context: %s")
  "Voice = noVoiceOfThisName")
#(ly:expect-warning (ly:translate-cpp-warning-scheme
                     "cannot determine music length"))

\new Lyrics \lyricsto noVoiceOfThisName { Text }
