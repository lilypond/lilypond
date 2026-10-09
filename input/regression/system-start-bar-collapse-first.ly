\version "2.27.4"

\header {
  texidoc = "Setting @code{SystemStartBar@/.collapse-@/first-@/height} supports
printing a systemic bar line at the beginning of the score without printing one
after each line break."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  ragged-right = ##t
  \context {
    \Score
    \override SystemStartBar.collapse-first-height = 0
    \override SystemStartBar.thickness = 3
  }
}

%% collapse-first-height was motivated by Kievan scores.  See Issue 6772.
\new KievanVoice {
  \senzaMisura
  \*3 { r \break }
}
