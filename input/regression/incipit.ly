\version "2.21.0"

\header {
  texidoc = "Incipits can be printed using an @code{InstrumentName}
grob.  In the second line of the second score the @code{InstrumentName}
grob should appear left-aligned."
}

\score {
  \new Staff {
    %% All this would be shortcuted by an appropriate music function:
    \override Staff.InstrumentName.self-alignment-X = #RIGHT
    \override Staff.InstrumentName.self-alignment-Y = ##f
    \override Staff.InstrumentName.padding = #0
    \override Staff.InstrumentName.stencil =
      #(lambda (grob)
         (let* ((instrument-name (ly:grob-property grob 'long-text))
                (layout (ly:output-def-clone (ly:grob-layout grob)))
                (music
                  #{
                    \new MensuralStaff
                      \with { instrumentName = #instrument-name }
                      { \clef "petrucci-c1" c'4 d' e' f' }
                  #})
                (score (ly:make-score music))
                (indent (ly:output-def-lookup layout 'indent))
                (incipit-width
                  (ly:output-def-lookup layout 'incipit-width (* indent 0.5))))
           (ly:output-def-set-variable! layout 'indent (- indent incipit-width))
           (ly:output-def-set-variable! layout 'line-width indent)
           (ly:output-def-set-variable! layout 'ragged-right #f)
           (ly:score-add-output-def! score layout)
           (set! (ly:grob-property grob 'long-text)
                 (markup #:score score))
           (system-start-text::print grob)))

    %% the instrument name definition is separated:
    \set Staff.instrumentName = "Instrument"
    c'4 d' e' f'
    g'1
  }
  \layout {
    indent = 5\cm
    incipit-width = 3\cm
  }
}

\score {
  \new Staff
    \with { instrumentName = "Instrument" shortInstrumentName = "Instrument" }
  {
    \incipit { \clef "petrucci-c1" c'4 d' e' f' }
    c'4 d' e' f' \break g'1
  }
  \layout {
    \override Staff.InstrumentName.self-alignment-X = #LEFT
    indent = 5\cm
    short-indent = 5\cm
    incipit-width = 3\cm
  }
}
