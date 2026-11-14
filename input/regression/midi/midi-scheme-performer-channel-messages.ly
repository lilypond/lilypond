\version "2.27.6"

\header {
  texidoc = "Program change, pitch bend, polyphonic key pressure and channel
pressure messages can be generated from a Scheme performer with
@code{midi-@/announce-@/program-@/change}, @code{midi-@/announce-@/pitch-@/bend},
@code{midi-@/announce-@/poly-@/pressure} and
@code{midi-@/announce-@/channel-@/pressure}."
}

% Expected:
%   first time step:  C0 28 (program 40), then E0 00 40 (no bend)
%   second:           E0 00 50 (bend up), A0 3E 32 (key pressure on D4)
%   third:            E0 00 40 (no bend)
%   fourth:           D0 40 (channel pressure)

#(define (channel-message-test context)
   (let ((count 0))
     (make-performer
      ((process-music performer)
       (case count
         ((0)
          (midi-announce-program-change performer 40)
          (midi-announce-pitch-bend performer 8192))
         ((1)
          (midi-announce-pitch-bend performer 10240)
          (midi-announce-poly-pressure performer 62 50))
         ((2)
          (midi-announce-pitch-bend performer 8192))
         ((3)
          (midi-announce-channel-pressure performer 64)))
       (set! count (1+ count))))))

\score {
  \new Staff \with { \consists #channel-message-test } { c'4 d' e' f' }
  \layout { }
  \midi { }
}
