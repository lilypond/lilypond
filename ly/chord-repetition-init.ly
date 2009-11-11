\version "2.13.8"
%{
Two functions define the chord repetition behavior, and may
be invoked by the user to customize it.

ly:parser-set-repetition-symbol
  set the chord repetition shortcut.
  `q' is the default value set in this file.

ly:parser-set-repetition-function
  set the function that is invoked when a chord repetition symbol
  is encountered by the parser: a three argument function
  (previous-chord, duration, list of articulations) which is supposed
  to return a new chord.
  `default-repeat-chord' is the default function set in this file.
%}

#(define-public (default-repeat-chord previous-chord duration articulations)
   "Copy the previous chord, filter out events which are not notes, set the
chord duration, add articulations."
   (let ((new-chord (ly:music-deep-copy previous-chord)))
     (set! (ly:music-property new-chord 'elements)
           (append! articulations
                    (filter (lambda (event)
                              (eqv? (ly:music-property event 'name) 'NoteEvent))
                            (ly:music-property new-chord 'elements))))
     (for-each (lambda (event)
                 (if (ly:duration? (ly:music-property event 'duration))
                     (set! (ly:music-property event 'duration) duration)))
               (ly:music-property new-chord 'elements))
    new-chord))

#(ly:parser-set-repetition-symbol parser 'q)
#(ly:parser-set-repetition-function parser default-repeat-chord)
