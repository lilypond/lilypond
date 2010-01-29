%%% -*- Mode: Scheme -*-
\version "2.13.9"
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
%}

#(define-public (default-repeat-chord previous-chord location duration articulations)
   "Copy the previous chord, filter out events which are not notes, set
the chord duration, add articulations."
   ;; If previous-chord has an length property, then it means that it
   ;; has been processed by a music iterator.  In other words, the chord
   ;; has been memorized in an other music block, which is certainly not
   ;; what the user has intended.  In that case, raise a warning.
   (if (not (and (ly:music? previous-chord)
                 (null? (ly:music-property previous-chord 'length))))
       (ly:input-message location
                         (_ "No memorized chord in music block before chord repetition")))
   ;; Instead of copying the previous chord, then removing the
   ;; undesired elements (like articulations), a new empty chord is
   ;; built.  Then, the pitch found in the previous chord are added to
   ;; the new chord, without any "decoration" (e.g. cautionary
   ;; accidentals, fingerings, text scripts, articulations).
   (make-music
    'EventChord
    'origin location
    'elements (append! (filter identity
                               (map (lambda (event)
                                      (and (eqv? (ly:music-property event 'name) 'NoteEvent)
                                           (make-music
                                            'NoteEvent
                                            'pitch (ly:music-property event 'pitch)
                                            'duration duration)))
                                    (ly:music-property previous-chord 'elements)))
                       articulations)))

#(ly:parser-set-repetition-symbol parser 'q)
#(ly:parser-set-repetition-function parser default-repeat-chord)
