\version "2.27.6"

\header {
  texidoc = "A translator created with @code{make-performer} runs only when
MIDI output is generated, and it can announce MIDI control changes and text
events, which end up in the track and on the channel of the enclosing
@code{Staff}.  Here, each of two staves emits a different pan position before
every note, and the first staff also emits a marker."
}

% Expected in the MIDI file (check with a hex dump; lilymidi does not
% display controller messages correctly yet):
%   staff 1: B0 0A 00, B0 0A 20, B0 0A 40, B0 0A 60 (marker before the 3rd)
%   staff 2: B0 0A 7F, B0 0A 5F, B0 0A 3F, B0 0A 1F
% each placed at the time step of the corresponding note.

#(define (pan-stepper start step with-marker)
   (lambda (context)
     (let ((count 0))
       (make-performer
        ((process-music performer)
         ;; process-music also runs at the final time step at the end of
         ;; the music, so stop after the four notes.
         (when (< count 4)
           (ly:performer-announce-control-change
            performer #f 10 (+ start (* step count)))
           (if (and with-marker (= count 2))
               (ly:performer-announce-text
                performer #f 'marker "step three"))
           (set! count (1+ count))))))))

\score {
  <<
    \new Staff \with {
      midiInstrument = "flute"
      \consists #(pan-stepper 0 32 #t)
    }
    { c'4 d' e' f' }

    \new Staff \with {
      midiInstrument = "cello"
      \consists #(pan-stepper 127 -32 #f)
    }
    { \clef bass c4 b, a, g, }
  >>
  \layout { }
  \midi { }
}
