\version "2.11.35"

\header {
  doctitle = "Piano template with centered dynamics"
  lsrtags = "expressive-marks,piano-music,template"
  texidoc = "
Many piano scores have the dynamics centered between the two staves.
This requires a bit of tweaking to implement, but since the template is
right here, you don't have to do the tweaking yourself.
" }

upper = \relative c'' {
           \clef treble
           \key c \major
           \time 4/4
         
           a b c d
         }
         
         lower = \relative c {
           \clef bass
           \key c \major
           \time 4/4
         
           a2 c
         }
         
         dynamics = {
           s2\fff\> s4
           s\!\pp
         }
         
         pedal = {
           s2\sustainDown s2\sustainUp
         }
         
         \score {
           \new PianoStaff <<
             \new Staff = "upper" \upper
             \new Dynamics = "dynamics" \dynamics
             \new Staff = "lower" <<
               \clef bass
               \lower
             >>
             \new Dynamics = "pedal" \pedal
           >>
           \layout {
             \context {
               \type "Engraver_group"
               \name Dynamics
               \alias Voice % So that \cresc works, for example.
               \consists "Output_property_engraver"
         
               \override VerticalAxisGroup #'minimum-Y-extent = #'(-1 . 1)
               \override DynamicLineSpanner #'Y-offset = #0
	       pedalSustainStrings = #'("Ped." "*Ped." "*")
               pedalUnaCordaStrings = #'("una corda" "" "tre corde")
         
               \consists "Piano_pedal_engraver"
               \consists "Script_engraver"
               \consists "Dynamic_engraver"
               \consists "Text_engraver"
         
               \override TextScript #'font-size = #2
               \override TextScript #'font-shape = #'italic
         
               \consists "Skip_event_swallow_translator"
         
               \consists "Axis_group_engraver"
             }
             \context {
               \PianoStaff
               \accepts Dynamics
             }
           }
         }
         \score {
           \new PianoStaff <<
             \new Staff = "upper" << \upper \dynamics >>
             \new Staff = "lower" << \lower \dynamics >>
             \new Dynamics = "pedal" \pedal
           >>
           \midi {
             \context {
               \type "Performer_group"
               \name Dynamics
               \consists "Piano_pedal_performer"
             }
             \context {
               \PianoStaff
               \accepts Dynamics
             }
           }
         }

