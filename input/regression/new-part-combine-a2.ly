
\header {
    texidoc ="The a2 string is only printed on notes, not on rests,
and only after chords, solo or polyphony."
    }

vone = \notes \relative a' { R1*2 g2 r2 g2 r2 a4 r4 g
			 }
vtwo = \notes \relative a' { R1*2 g2 r2 g2 r2 f4 r4 g }

\score {
    << \property Score.skipBars = ##t 
   \newpartcombine \vone \vtwo
       >>
}
 
