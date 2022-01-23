\version "2.23.6"

\layout {
  ragged-right = ##t
}

voiceMusic = {
  \sectionLabel "R1*7" R1*7
  \sectionLabel "R1" R1
  \sectionLabel "r1" r1
  \sectionLabel "r2" r2
  \sectionLabel "r4" r4*2
}

\score {
  {
    \compressMMRests
    \new StaffGroup <<
      $@(map
      (lambda (n) #{
        %% The including file will define staffMusic
        \new Staff \with {
          \clef "alto"
          \override StaffSymbol.line-count = $n
        } {
          \makeStaffMusic \voiceMusic
        } #})
      (iota 8))
      $@(map
      (lambda (x) #{
        \new TabStaff \with { stringTunings = #x } {
          \makeStaffMusic \voiceMusic
        } #})
      (list mandolin-tuning banjo-c-tuning guitar-tuning))
    >>
  }
  \layout { \tabFullNotation }
}
