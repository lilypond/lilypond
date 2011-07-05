\version "2.14.0"

#(define (filter-instrument instrument-name tuning-alist)
   (filter (lambda (entry)
             (string-prefix? instrument-name
                            (symbol->string (car entry))))
           tuning-alist))

#(define (chord-display tuning-alist-entry)
   (let* ((ch-mus (cdr tuning-alist-entry))
          (tuning-symbol (car tuning-alist-entry))
          (ev-chord (car (extract-named-music
                           ch-mus
                           'EventChord)))
          (elts (ly:music-property ev-chord 'elements)))
     (music-map (lambda (m)
                  (begin
                    (if (not (null? (ly:music-property m 'duration)))
                        (ly:music-set-property!
                          m
                          'duration
                          (ly:make-duration 0 0 1 1)))
                    m))
                ev-chord)
     (let ((elts (ly:music-property ev-chord 'elements))
           (script (make-music 'TextScriptEvent
                               'direction 1
                               'text (symbol->string tuning-symbol))))
       (ly:music-set-property!
         ev-chord
         'elements
         (cons script elts)))
     ev-chord))

displayInstrumentDefaultTunings =
#(define-music-function (parser location instrument) (string?)
   (let* ((filtered-instruments (filter-instrument instrument defaultStringTunings))
          (display-elements (map chord-display filtered-instruments)))
     (make-music 'SequentialMusic 'elements display-elements)))


\score {
  {
    \new Staff {
      \textLengthOn
      \override Score.RehearsalMark #'self-alignment-X = #LEFT

      \mark \markup {\left-align "Guitar tunings"}
      \clef "treble_8"
      \displayInstrumentDefaultTunings #"guitar"
      \break

      \mark \markup {\left-align "Bass tunings"}
      \clef "bass_8"
      \displayInstrumentDefaultTunings #"bass"
      \break

      \mark \markup {\left-align "Mandolin tunings"}
      \clef "treble"
      \displayInstrumentDefaultTunings #"mandolin"
      \break

      \mark \markup {\left-align "Banjo tunings"}
      \clef "treble_8"
      \displayInstrumentDefaultTunings #"banjo"
      \break

      \mark \markup {\left-align "Ukulele tunings"}
      \clef "treble"
      \displayInstrumentDefaultTunings #"ukulele"
      \clef "alto"
      \displayInstrumentDefaultTunings #"tenor-ukulele"
      \displayInstrumentDefaultTunings #"baritone-ukulele"
      \break

      \mark \markup {\left-align "Orchestral string tunings"}
      \clef "treble"
      \displayInstrumentDefaultTunings #"violin"
      \clef "alto"
      \displayInstrumentDefaultTunings #"viola"
      \clef "bass"
      \displayInstrumentDefaultTunings #"cello"
      \clef "bass_8"
      \displayInstrumentDefaultTunings #"double-bass"
    }
  }
}
