
\header {
    texidoc ="The new part combiner.
Apart for:
@itemize @bullet
@item different durations (start points)
@item different articulations (only slur/beam/tie work)
@item wide pitch ranges
@end itemize
"
    }

theMusic = \context Staff \notes { c4 d8-. }



vone = \notes \relative a' { g2 g    g  g4 g f' c c( c) c c  c ~ c
			     c2. c4   c
			 }
vtwo = \notes \relative a' { f2 f4 f f2 g4 g c, f f  f  f f~ f ~ f
			     f4 f2. ~ f4

			 }

\score {
   \newpartcombine \vone \vtwo
}
 
