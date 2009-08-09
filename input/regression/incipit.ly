\version "2.13.4"

\header {
  texidoc = "Incipits can be printed using an @code{InstrumentName}
grob."
}

%% to prevent warnings/programming errors:
#(set-object-property! 'music 'backend-type? ly:music?)
#(set-object-property! 'music 'backend-doc "Incipit music")
#(ly:add-interface 'incipit-interface "An incipit." '(music))
#(let* ((instrument-def (assoc 'InstrumentName all-grob-descriptions))
        (meta-def (assoc 'meta (cdr instrument-def)))
        (interfaces-def (assoc 'interfaces (cdr meta-def)))
        (interfaces (cdr interfaces-def)))
   (set-cdr! interfaces-def (cons 'incipit-interface interfaces)))

\score {
  \new Staff {
    %% All this would be shortcuted by an appropriate music function:
    \override Staff.InstrumentName #'music = ##{ \clef "petrucci-c1" c'4 d' e' f' #}
    \override Staff.InstrumentName #'self-alignment-X = #RIGHT
    \override Staff.InstrumentName #'padding = #0
    \override Staff.InstrumentName #'stencil =
    #(lambda (grob)
       (let* ((instrument-name (ly:grob-property grob 'long-text))
              (layout (ly:output-def-clone (ly:grob-layout grob)))
              (music (make-music 'SequentialMusic
                      'elements (list (make-music 'ContextSpeccedMusic
                                        'context-type 'MensuralStaff
                                        'element (make-music 'PropertySet
                                                   'symbol 'instrumentName
                                                   'value instrument-name))
                                      (ly:grob-property grob 'music))))
              (score (ly:make-score music))
              (mm (ly:output-def-lookup layout 'mm))
              (indent (ly:output-def-lookup layout 'indent))
              (incipit-width (ly:output-def-lookup layout 'incipit-width))
              (scaled-incipit-width (if (number? incipit-width)
                                        (* incipit-width mm)
                                        (* indent 0.5))))
         (ly:output-def-set-variable! layout 'indent (- indent scaled-incipit-width))
         (ly:output-def-set-variable! layout 'line-width indent)
         (ly:output-def-set-variable! layout 'ragged-right #f)
         (ly:score-add-output-def! score layout)
         (set! (ly:grob-property grob 'long-text)
               (markup #:score score))
         (system-start-text::print grob)))

    %% the instrument name definition is separated:
    \set Staff.instrumentName = #"Instrument"
    c'4 d' e' f' g'1
  }
  \layout {
    indent = 5\cm
    incipit-width = 3\cm
  }
}