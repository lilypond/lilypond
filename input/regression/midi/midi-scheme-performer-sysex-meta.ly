\version "2.27.6"

\header {
  texidoc = "System exclusive messages and meta events can be generated from
a Scheme performer with @code{ly:performer-@/announce-@/midi-@/sysex} and
@code{ly:performer-@/announce-@/midi-@/meta}.  Data can be given as a list of
integers or as a bytevector."
}

% Expected, first time step:
%   F0 05 7E 7F 09 01 F7              GM system on (F7 is added)
%   FF 09 07 72 65 67 74 65 73 74     device name "regtest"
%   FF 7F 03 7D 01 02                 sequencer specific, non-commercial id
% second time step: the same sysex again, given as a bytevector with the
% terminating F7 already present.

#(use-modules (rnrs bytevectors))

#(define (sysex-meta-test context)
   (let ((count 0))
     (make-performer
      ((process-music performer)
       (case count
         ((0)
          (ly:performer-announce-midi-sysex
           performer #f '(#x7e #x7f #x09 #x01))
          (ly:performer-announce-midi-meta
           performer #f #x09
           (map char->integer (string->list "regtest")))
          (ly:performer-announce-midi-meta
           performer #f #x7f '(#x7d #x01 #x02)))
         ((1)
          (ly:performer-announce-midi-sysex
           performer #f
           (u8-list->bytevector '(#x7e #x7f #x09 #x01 #xf7)))))
       (set! count (1+ count))))))

\score {
  \new Staff \with { \consists #sysex-meta-test } { c'4 d' e' f' }
  \layout { }
  \midi { }
}
