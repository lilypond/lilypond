% junkme?
\version "2.3.8"
\header {
  texidoc = "@cindex Beam Dir Functions

The direction of a beam may be calculated in several ways. As shown in
the example, the beam are be below the notes if:
@table @code
@item majority
of (individual) notes would have down stems,
@item mean
of note pitches is on the center line or below it, or
@item median
of note pithes (i.e. the centermost element of ordered pitches) is 
on the center line or below it.
@end table

If your favourite algorithm is not one of these, you can hook up your 
own one. 
(These beam direction functions are defined in @file{scm/beam.scm}.)  
"
}

\paper { raggedright = ##t}
\score {
  \relative c'' {\time 3/4
    \override Beam  #'dir-function = #beam-dir-majority
      c8[ g]
    \override Beam  #'dir-function = #beam-dir-mean
      c[ g] 
    \override Beam  #'dir-function = #beam-dir-median
      c[ g]
    
    \time 3/8
    \override Beam  #'dir-function = #beam-dir-majority
      c8[ c g]
    \override Beam  #'dir-function = #beam-dir-mean
      c[ c g] 
    \override Beam  #'dir-function = #beam-dir-median
      c[ c g] 
  }
\paper{raggedright = ##t}
}

%% Local variables:
%% LilyPond-indent-level:2
%% End:



