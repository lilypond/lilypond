\header {

  texidoc = "scripts don't trigger beam formatting.  If this
does happen, we can have a cyclic dependency on Y-positions of
staves."

}


\version "2.16.0"

\new PianoStaff <<
   \new Staff = RH {
      \time 1/4
      c''16 [
      c''16
      \change Staff = LH
      c''16 \tenuto _ \markup { foo }
      \change Staff = RH
      c''16 ]
   }
   \new Staff = LH {
      s4
   }
>>

%%% END %%%

