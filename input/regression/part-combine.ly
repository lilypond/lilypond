
\header {
  texidoc ="The new part combiner stays apart from:
@itemize @bullet
@item different durations,
@item different articulations (taking into account only slur/@/beam/@/tie), and
@item wide pitch ranges.
@end itemize
"
}

\layout { ragged-right = ##t }

\version "2.21.0"

vone =  \relative {
  g'2 g    g  g4 g f' c c( c) c c  c ~ c
  c2. c4   c
}
vtwo =  \relative {
  f'2 f4 f f2 g4 g c, f f  f  f f~ f ~ f
  f4 f2. ~ 4
}


\partCombine \vone \vtwo

 

 
