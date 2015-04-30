%\version "2.19.19"

\include "staff-tkit.ly"

make-pianostaff =
#(define-music-function (parser location) ()

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
    \override VerticalAxisGroup.remove-empty = ##t
    \override VerticalAxisGroup.remove-first = ##t
  }
  <<
    \make-one-voice-staff ##f "PianoRH" "treble" ""
    #(if PianoDynamics
         #{ \new Dynamics = "PianoDynamics" { #PianoDynamics } #} )
    \make-one-voice-staff ##f "PianoLH" "bass" ""
  >>
       #}
     (make-music 'SequentialMusic 'void #t)))
