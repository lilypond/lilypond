\header { texidoc= "

Whenever a voice switches to another staff a line connecting the notes
can be printed automatically. This is enabled if the property
@code{Thread.followVoice} is set to true. "

}
\score {
\notes \relative c'
\context PianoStaff <
    \property PianoStaff.followVoice = ##t
    \context Staff \context Voice {
      c1
      \translator Staff=two
      b2 a
    }
    \context Staff=two {\clef bass \skip 1*2 }
  >\paper{ 
linewidth =-1.
}}
