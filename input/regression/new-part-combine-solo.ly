
\header { texidoc =

	  "A solo string can only be printed when a note
    starts. Hence, in this example, there is no Solo-2 although the
    2nd voice has a dotted quarter, while the first voice has a rest.

A Solo indication is only printed once; (shared) rests do not require
reprinting a solo indication.

Solo 1/2 can not be used when a spanner is active, so there is no solo
 over any of the tied notes.

"
    }

vone = \notes \relative a' { g4 r8 g8 g8 r8 g8 r8 g2 ~ g2 ~ g4 }
vtwo = \notes \relative g' { e4.   e8 r2          e4 r4 r2  e4 } 

\score {
    << \property Score.skipBars = ##t 
   \newpartcombine \vone \vtwo
       >>
}
 
