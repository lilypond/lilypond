\version "2.23.6"

\layout {
  %% identify each staff in the margin
  \context {
    \Staff
    instrumentName = "Staff"
  }
  \context {
    \GregorianTranscriptionStaff
    instrumentName = \markup \column { Gregor. Transcr. }
  }
  \context {
    \KievanStaff
    instrumentName = "Kievan"
  }
  \context {
    \MensuralStaff
    instrumentName = "Mensural"
  }
  \context {
    \PetrucciStaff
    instrumentName = "Petrucci"
  }
  \context {
    \VaticanaStaff
    instrumentName = "Vaticana"
  }
}

\paper {
  %% tighten up the multi-score tests
  score-system-spacing.basic-distance = #9
  score-system-spacing.minimum-distance = #0
  score-system-spacing.padding = #0
}

staffA = \new Staff << \divisions \labels \music >>
staffB = \new GregorianTranscriptionStaff << \divisions \music >>
staffC = \new KievanStaff << \divisions \music >>
staffD = \new MensuralStaff << \divisions \music >>
staffE = \new PetrucciStaff << \divisions \music >>
staffF = \new VaticanaStaff << \divisions \music >>

staffGroup = \new StaffGroup \with { \unset systemStartDelimiter } <<
  %% placing the divisions and labels in only one staff is intentional
  \new Staff << \divisions \labels \music >>
  \new GregorianTranscriptionStaff \music
  \new KievanStaff \music
  \new MensuralStaff \music
  \new PetrucciStaff \music
  \new VaticanaStaff \music
>>
