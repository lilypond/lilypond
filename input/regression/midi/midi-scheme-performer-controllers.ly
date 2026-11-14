\version "2.27.6"

\header {
  texidoc = "The procedures @code{midi-@/announce-@/controller},
@code{midi-@/announce-@/controller-14bit}, @code{midi-@/announce-@/rpn} and
@code{midi-@/announce-@/nrpn} generate MIDI controller messages from a Scheme
performer.  Named controllers, 14-bit controllers, the pitch bend range
(registered parameter 0) and a non-registered parameter are used at the first
time step; the sustain pedal is pressed at the third and released at the
fourth."
}

% Expected, first time step:
%   B0 07 64              volume 100
%   B0 0A 40              pan 64
%   B0 0B 24, B0 2B 34    expression 4660 as 14 bits
%   B0 5B 28              reverb 40
%   B0 65 00, B0 64 00, B0 06 0C, B0 26 00, B0 65 7F, B0 64 7F
%                         pitch bend range 12 semitones
%   B0 63 09, B0 62 52, B0 06 2C, B0 26 2E, B0 65 7F, B0 64 7F
%                         NRPN 1234 = 5678
% third time step: B0 40 7F; fourth: B0 40 00.

#(define (controller-test context)
   (let ((count 0))
     (make-performer
      ((process-music performer)
       (case count
         ((0)
          (midi-announce-controller performer 'volume 100)
          (midi-announce-controller performer 'pan 64)
          (midi-announce-controller-14bit performer 'expression 4660)
          (midi-announce-controller performer 'reverb 40)
          (midi-announce-pitch-bend-range performer 12)
          (midi-announce-nrpn performer 1234 5678))
         ((2)
          (midi-announce-controller performer 'sustain 127))
         ((3)
          (midi-announce-controller performer 'sustain 0)))
       (set! count (1+ count))))))

\score {
  \new Staff \with { \consists #controller-test } { c'4 d' e' f' }
  \layout { }
  \midi { }
}
