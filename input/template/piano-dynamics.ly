\version "2.3.4"
\header {
texidoc ="
  Dynamics on a separate line, neatly centered between staffs.
"
}

upper = \relative c'' {
  a b c d
}

lower = \relative c {
  a2 c
}

dynamics =  {
  s2\fff\> s4
  s\!\pp
}

pedal =  {
 s2\sustainDown s2\sustainUp
}

\score {
  \context PianoStaff <<
    \context Staff=upper \upper
    \context Dynamics=dynamics \dynamics
    \context Staff=lower <<
      \clef bass
      \lower
    >>
    \context Dynamics=pedal \pedal
  >>
  \paper {
    \context {
      \type "Engraver_group_engraver"
      \name Dynamics
      \alias Voice % So that \cresc works, for example.
      \consists "Output_property_engraver"
      
      minimumVerticalExtent = #'(-1 . 1)
      pedalSustainStrings = #'("Ped." "*Ped." "*")
      pedalUnaCordaStrings = #'("una corda" "" "tre corde")
      
      \consists "Piano_pedal_engraver"
      \consists "Script_engraver"
      \consists "Dynamic_engraver"
      \consists "Text_engraver"

      \override TextScript #'font-size = #2
      \override TextScript #'font-shape = #'italic
      \override DynamicText #'extra-offset = #'(0 . 2.5)
      \override Hairpin #'extra-offset = #'(0 . 2.5)

      \consists "Skip_event_swallow_translator"

      \consistsend "Axis_group_engraver"
    }
    \context {
      \PianoStaff
      \accepts Dynamics
      \override VerticalAlignment #'forced-distance = #7
    }
  }
  \midi {
    \context {
      \type "Performer_group_performer"
      \name Dynamics
      \consists "Piano_pedal_performer"
      \consists "Span_dynamic_performer"
      \consists "Dynamic_performer"
    }
    \context {
      \PianoStaff
      \accepts Dynamics
    }
  }
}
