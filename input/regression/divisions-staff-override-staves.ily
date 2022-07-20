\version "2.23.12"

\layout {
  %% Color the Divisio grobs because it is otherwise hard to tell some
  %% of them apart from BarLine grobs.
  \context {
    \Score
    \override Divisio.color = #(universal-color 'blue)
  }

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
  %% placing the labels in only one staff is intentional
  \new Staff << \divisions \labels \music >>
  \new GregorianTranscriptionStaff << \divisions \music >>
  \new KievanStaff << \divisions \music >>
  \new MensuralStaff << \divisions \music >>
  \new PetrucciStaff << \divisions \music >>
  \new VaticanaStaff << \divisions \music >>
>>
