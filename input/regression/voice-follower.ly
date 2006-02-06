\version "2.7.32"


\header { texidoc= "

Whenever a voice switches to another staff a line connecting the notes
can be printed automatically. This is enabled if the property
@code{followVoice} is set to true. "

}

\layout{ 
  ragged-right = ##t 
}

\relative c'
\context PianoStaff <<
  \set PianoStaff.followVoice = ##t
  \context Staff = "one" \context Voice {
    c1
    \change Staff=two
    b2 a
  }
  \context Staff = "two" {\clef bass \skip 1*2 }
>>


