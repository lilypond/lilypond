\version "2.27.6"

\header {
  categories = "Midi, Scheme"

  texidoc = "
A Scheme performer, created with @code{make-performer}, can send MIDI
controller messages.  Here, volume, pan position and reverb level are set at
the start, and the sustain pedal controller is switched on and off later.
The variable @code{count} counts the time steps in which something happens,
here the notes.  The messages are written to the MIDI file; the score only
shows the notes.
"

  doctitle = "Sending MIDI controller messages from a Scheme performer"
}

#(define (controller-example context)
   (let ((count 0))
     (make-performer
      ((process-music performer)
       (case count
         ((0)
          (midi-announce-controller performer 'volume 100)
          (midi-announce-controller performer 'pan 32)   ; left of centre
          (midi-announce-controller performer 'reverb 60))
         ((2)
          (midi-announce-controller performer 'sustain 127))
         ((4)
          (midi-announce-controller performer 'sustain 0)))
       (set! count (1+ count))))))

\score {
  \new Staff { c'4 d' e' f' g' a' b' c'' }
  \layout { }
  \midi {
    \context {
      \Staff
      \consists #controller-example
    }
  }
}
