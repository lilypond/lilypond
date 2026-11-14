\version "2.27.6"

\header {
  categories = "Midi, Scheme"

  texidoc = "
System exclusive messages, here a General MIDI system on message followed by
a universal real-time message that sets the master volume, can be sent
with @code{ly:performer-@/announce-@/midi-@/sysex}.  The bytes are given without the
leading @code{#xF0}; the terminating @code{#xF7} is added automatically.
"

  doctitle = "Sending system exclusive messages from a Scheme performer"
}

#(define (sysex-example context)
   (let ((done #f))
     (make-performer
      ((process-music performer)
       (if (not done)
           (begin
             ;; General MIDI system on
             (ly:performer-announce-midi-sysex
              performer #f '(#x7e #x7f #x09 #x01))
             ;; master volume: LSB #x00, MSB #x60, i.e. 12288 of at most 16383
             (ly:performer-announce-midi-sysex
              performer #f '(#x7f #x7f #x04 #x01 #x00 #x60))
             (set! done #t)))))))

\score {
  \new Staff { c'4 d' e' f' }
  \layout { }
  \midi {
    \context {
      \Staff
      \consists #sysex-example
    }
  }
}
