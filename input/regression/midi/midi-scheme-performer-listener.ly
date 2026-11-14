\version "2.27.6"

\header {
  texidoc = "A performer can listen to events and generate MIDI messages
from them.  Here, the modulation wheel follows the pitch of the notes: before
each note, a modulation controller message is emitted whose value is 64 plus
the number of semitones above middle C."
}

% Expected, one message at the time step of each note:
%   B0 01 40, B0 01 42, B0 01 44, B0 01 45

#(define (pitch-to-modulation context)
   (let ((pending #f))
     (make-performer
      (listeners
       ((note-event performer event)
        (set! pending
              (ly:pitch-semitones (ly:event-property event 'pitch)))))
      ((process-music performer)
       (if pending
           (begin
             (midi-announce-controller performer 'modulation (+ 64 pending))
             (set! pending #f)))))))

\score {
  \new Staff \with { \consists #pitch-to-modulation } { c'4 d' e' f' }
  \layout { }
  \midi { }
}
