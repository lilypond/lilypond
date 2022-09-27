\version "2.23.14"

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
     (let* ((filtered-instruments
             (filter-instrument instrument defaultStringTunings))
            (display-elements (map chord-display filtered-instruments)))
       (make-music 'SequentialMusic 'elements display-elements)))


{
  \textMark \markup \huge \bold "Guitar tunings"
  \clef "treble_8"
  \displayInstrumentDefaultTunings "guitar"
}

{
  \textMark \markup \huge \bold "Bass tunings"
  \clef "bass_8"
  \displayInstrumentDefaultTunings "bass"
}

{
  \textMark \markup \huge \bold "Mandolin tunings"
  \displayInstrumentDefaultTunings "mandolin"
}

{
  \textMark \markup \huge \bold "Banjo tunings"
  \clef "treble_8"
  \displayInstrumentDefaultTunings "banjo"
}

{
  \textMark \markup \huge \bold "Ukulele tunings"
  \clef treble
  \displayInstrumentDefaultTunings "ukulele"
  \clef alto
  \displayInstrumentDefaultTunings "tenor-ukulele"
  \displayInstrumentDefaultTunings "baritone-ukulele"
}

{
  \textMark \markup \huge \bold "Orchestral string tunings"
  \clef treble
  \displayInstrumentDefaultTunings "violin"
  \clef alto
  \displayInstrumentDefaultTunings "viola"
  \clef bass
  \displayInstrumentDefaultTunings "cello"
  \clef "bass_8"
  \displayInstrumentDefaultTunings "double-bass"
}


\layout {
  \context {
    \Score
    \omit BarNumber
    \override RehearsalMark.break-align-symbols = #'(left-edge)
    \override RehearsalMark.self-alignment-X = #LEFT
    \override RehearsalMark.padding = #4
  }

  \context {
    \Voice
    \textLengthOn
  }
}


\paper {
  ragged-right = ##t
}
