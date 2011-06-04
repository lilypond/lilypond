%%% -*- Mode: Scheme -*-
\version "2.14.0"
%{

The following functions define the chord repetition behavior, and may
be invoked by the user to customize it.

ly:parser-set-repetition-symbol
  set the chord repetition shortcut.
  `q' is the default value set in this file.

ly:parser-set-repetition-function

  set the function that is invoked when a chord repetition symbol
  is encountered by the parser: a four argument function
  (previous-chord, location, duration, list of articulations) which is
  supposed to return a new chord.
  `default-repeat-chord' is the default function set in this file.
  `tab-repeat-chord' may be used in tablatures to preserve the string information.
%}

#(define-public ((make-repeat-chord-function chord-element-types note-articulation-types)
                 previous-chord location duration articulations)
   "Make a chord repetition function.
The returned functions copies the notes from @var{previous-chord} into a new chord.
Chord elements, which type is found in @var{chord-element-types}, are copied into
the new chord. Note articulations, which type is found in @var{note-articulation-types},
are also copied. All other events are not copied into the new chord."
   (define (filter-events events event-types)
     (filter (lambda (event)
	       (and (memq (ly:music-property event 'name) event-types) event))
	     events))
   ;; If previous-chord has an length property, then it means that it
   ;; has been processed by a music iterator.  In other words, the chord
   ;; has been memorized from an other music block, which is certainly not
   ;; what the user has intended, as anywy the result will be buggy.
   ;; In that case, raise a warning.
   (if (not (and (ly:music? previous-chord)
		 (null? (ly:music-property previous-chord 'length))))
       (ly:input-message location
			 (_ "No memorized chord in music block before chord repetition")))
   ;; Instead of copying the previous chord, then removing the
   ;; undesired elements (like articulations), a new empty chord is built.
   ;; Then, the pitch found in the previous chord are added to the new
   ;; chord, without any "decoration" (e.g. cautionary accidentals,
   ;; fingerings, text scripts, articulations).  Only the events of types
   ;; given in `chord-elements-types' and `note-articulation-types' are
   ;; copied from the original chord elements and note articulations,
   ;; respectively.
   (let ((elements (ly:music-property (ly:music-deep-copy previous-chord) 'elements)))
     (make-music
      'EventChord
      'origin location
      'elements (append!
                 (map (lambda (note)
                        (let ((new-note (make-music 'NoteEvent
                                                    'origin location
                                                    'pitch (ly:music-property note 'pitch)
                                                    'duration duration))
                              (articulations
                               (filter-events (ly:music-property note 'articulations)
                                              note-articulation-types)))
                          (if (not (null? articulations))
                              (set! (ly:music-property new-note 'articulations)
                                    articulations))
                          new-note))
                      (filter-events elements '(NoteEvent)))
                 (filter-events elements chord-element-types)
                 articulations))))

#(define-public default-repeat-chord
   (make-repeat-chord-function '() '()))

#(define-public tab-repeat-chord
   (make-repeat-chord-function '(StringNumberEvent) '(StringNumberEvent)))

% default settings
#(ly:parser-set-repetition-symbol parser 'q)
#(ly:parser-set-repetition-function parser default-repeat-chord)
