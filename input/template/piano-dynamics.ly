\version "1.5.1"

\header {
  dedication = "dedication"
  title = "title"
  subtitle = "subtitle"
  subsubtitle = "subsubtitle"
  composer = "composer (xxxx-yyyy)"
  opus = "opus 0"
  piece = "piece i"
  instrument = "instrument"
  arranger = "arranger"
  poet = "poet"
  texttranslator = "translator"
  copyright = "public domain"
  enteredby = "jcn"
  source =  "urtext"
}

upper = \notes\relative c'' {
  a b c d
}

lower = \notes\relative c {
  a2 c
}

dynamics = \notes {
  s2\fff\> s4
  \!s\pp
}

pedal = \notes {
 s2\sustaindown s2\sustainup
}

\score {
  \context pianostaff <
    \context staff=upper \upper
    \context dynamics=dynamics \dynamics
    \context staff=lower <
      \clef bass
      \lower
    >
    \context dynamics=pedal \pedal
  >
  \paper {
    \translator {
      \type "engraver_group_engraver"
      \name dynamics
      \consists "output_property_engraver"
      Generic_property_list = #generic-voice-properties
      \consists "Property_engraver"
      MinimumVerticalExtent = #'(-1 . 1)

      pedalSustainStrings = #'("Ped." "*Ped." "*")
      pedalUnaCordaStrings = #'("una corda" "" "tre corde")
      
      \consists "Piano_pedal_engraver"
      \consists "Script_engraver"
      \consists "Dynamic_engraver"
      \consists "Text_engraver"

      TextScript \override #'font-relative-size = #1
      TextScript \override #'font-shape = #'italic
      DynamicText \override #'extra-offset = #'(0 . 2.5)
      Hairpin \override #'extra-offset = #'(0 . 2.5)

      \consists "Skip_req_swallow_translator"

      \consistsend "Axis_group_engraver"
    }
    \translator {
      \PianoStaffContext
      \accepts Dynamics
      VerticalAlignment \override #'forced-distance = #7
    }
  }
  \midi {
    \translator {
      \type "Performer_group_performer"
      \name Dynamics
      Generic_property_list = #generic-voice-properties

      \consists "Piano_pedal_performer"
      \consists "Span_dynamic_performer"
      \consists "Dynamic_performer"
    }
    \translator {
      \PianoStaffContext
      \accepts Dynamics
    }
  }
}
