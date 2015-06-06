%\version "2.19.22"

\include "staff-tkit.ly"

make-pianostaff =
#(define-music-function () ()

(if (not PianoRHMidiInstrument)
       (set! PianoRHMidiInstrument
             (if PianoMidiInstrument
                 PianoMidiInstrument
                 "acoustic grand")))

(if (not PianoLHMidiInstrument)
       (set! PianoLHMidiInstrument
             (if PianoMidiInstrument
                 PianoMidiInstrument
                 "acoustic grand")))

  (if (or
        PianoRHMusic
        PianoLHMusic)
       #{

\new PianoStaff = "PianoStaff"
  \with {
    instrumentName = \markup \smallCaps
      #(if PianoInstrumentName
           PianoInstrumentName
           "Piano" )
    shortInstrumentName = \markup \smallCaps
      #(if PianoShortInstrumentName
           PianoShortInstrumentName
           "")
  }
  <<
    \make-one-voice-staff ##f "PianoRH" "treble" ""
    #(if PianoDynamics
         #{ \new Dynamics = "PianoDynamics" { #PianoDynamics } #} )
    \make-one-voice-staff ##f "PianoLH" "bass" ""
  >>
       #}
     (make-music 'SequentialMusic 'void #t)))
