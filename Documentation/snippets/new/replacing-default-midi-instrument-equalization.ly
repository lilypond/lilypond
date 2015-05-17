\version "2.19.21"

\header {

  lsrtags = "scheme-language, midi"

  texidoc = "The default MIDI instrument equalizer can be replaced by
  setting the @code{instrumentEqualizer} property in the @code{Score}
  context to a user-defined Scheme procedure that uses a MIDI instrument
  name as its argument along with a pair of fractions indicating the
  minimum and maximum volumes respectively to be applied to that
  specific instrument.

  The following example sets the minimum and maximum volumes for flute
  and clarinet respectively."

  doctitle = "Replacing default MIDI instrument equalization"
}

#(define my-instrument-equalizer-alist '())

#(set! my-instrument-equalizer-alist
  (append
    '(
      ("flute" . (0.7 . 0.9))
      ("clarinet" . (0.3 . 0.6)))
    my-instrument-equalizer-alist))

#(define (my-instrument-equalizer s)
  (let ((entry (assoc s my-instrument-equalizer-alist)))
    (if entry
      (cdr entry))))

\score {
  <<
    \new Staff {
      \key g \major
      \time 2/2
      \set Score.instrumentEqualizer = #my-instrument-equalizer
      \set Staff.midiInstrument = #"flute"
      \new Voice \relative {
        r2 g''\mp g fis~
        4 g8 fis e2~
        4 d8 cis d2
      }
    }
    \new Staff {
      \key g \major
      \set Staff.midiInstrument = #"clarinet"
      \new Voice \relative {
        b'1\p a2. b8 a
        g2. fis8 e
        fis2 r
      }
    }
  >>
  \layout { }
  \midi {  }
}
