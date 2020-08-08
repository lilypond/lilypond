\version "2.19.22"

#(define (filter-instrument instrument-name tuning-alist)
   (filter (lambda (entry)
             (string-prefix? instrument-name
                            (symbol->string (car entry))))
           tuning-alist))

#(define (chord-display tuning-alist-entry)
   (let* ((tuning-symbol (car tuning-alist-entry))
	  (pitches (cdr tuning-alist-entry)))
     (make-music 'EventChord
		 'elements
		 (cons (make-music 'TextScriptEvent
				   'direction 1
				   'text (symbol->string tuning-symbol))
		       (map (lambda (pitch)
			      (make-music 'NoteEvent
					  'duration (ly:make-duration 0 0 1/1)
					  'pitch pitch))
			    pitches)))))

displayInstrumentDefaultTunings =
#(define-music-function (instrument) (string?)
   (let* ((filtered-instruments (filter-instrument instrument defaultStringTunings))
          (display-elements (map chord-display filtered-instruments)))
     (make-music 'SequentialMusic 'elements display-elements)))


\score {
  {
    \new Staff {
      \textLengthOn
      \override Score.RehearsalMark.self-alignment-X = #LEFT

      \mark \markup {\left-align "Guitar tunings"}
      \clef "treble_8"
      \displayInstrumentDefaultTunings "guitar"
      \break

      \mark \markup {\left-align "Bass tunings"}
      \clef "bass_8"
      \displayInstrumentDefaultTunings "bass"
      \break

      \mark \markup {\left-align "Mandolin tunings"}
      \clef "treble"
      \displayInstrumentDefaultTunings "mandolin"
      \break

      \mark \markup {\left-align "Banjo tunings"}
      \clef "treble_8"
      \displayInstrumentDefaultTunings "banjo"
      \break

      \mark \markup {\left-align "Ukulele tunings"}
      \clef "treble"
      \displayInstrumentDefaultTunings "ukulele"
      \clef "alto"
      \displayInstrumentDefaultTunings "tenor-ukulele"
      \displayInstrumentDefaultTunings "baritone-ukulele"
      \break

      \mark \markup {\left-align "Orchestral string tunings"}
      \clef "treble"
      \displayInstrumentDefaultTunings "violin"
      \clef "alto"
      \displayInstrumentDefaultTunings "viola"
      \clef "bass"
      \displayInstrumentDefaultTunings "cello"
      \clef "bass_8"
      \displayInstrumentDefaultTunings "double-bass"
    }
  }
}
