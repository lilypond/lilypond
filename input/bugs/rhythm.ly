text =   \lyrics { four4 syl- la ble }
two = \notes  { c4. c8 }
fourn = \notes  { c4. c8 c4. c8 }
fiven = \notes  { [c16  c16 \bar "";  <{ c8 } { c16 c16 }> c8 }

\score { \notes{
	\context Staff = SA
	< \context Staff { \two } 
 	  \context Lyrics { \rhythm \fourn \text   }
	>
	< \context Staff { \two } 
 	  \context Lyrics { \rhythm \two \text   }
	>
	< \context Staff { \fiven } 
 	  \context Lyrics { \rhythm \fiven \text   }
	>

}
}
