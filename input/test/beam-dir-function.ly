\version "1.5.68"

\header{
  texidoc="

There are several ways to calculate the direction of a beam

@table @samp
@item majority
number count of up or down notes
@item mean
mean centre distance of all notes
@item median
mean centre distance weighted per note
@end table

These beam direction functions are defined in @file{scm/beam.scm}.  If
your favourite algorithm isn't one of these, you can hook up your own.

Of course, this depends on the neutral-direction for the middle line,
down by default.  We set that to 1 (up) in the lower staff."  }

\score {
  \context PianoStaff <
    \context Staff \notes \relative c'' {
      \property Voice.Beam \set #'dir-function = #beam-dir-majority
      [c8^"down" g]
      \property Voice.Beam \set #'dir-function = #beam-dir-mean
      [c^"up" g] 
      \property Voice.Beam \set #'dir-function = #beam-dir-median
      [c^"up" g]
      
      \time 3/8
      \property Voice.Beam \set #'dir-function = #beam-dir-majority
      [c8^"down" c g]
      \property Voice.Beam \set #'dir-function = #beam-dir-mean
      [c^"down" c g] 
      \property Voice.Beam \set #'dir-function = #beam-dir-median
      [c^"up" c g]
    }
    \context Staff=lower \notes \relative c'' {
      \property Voice.Beam \set #'neutral-direction = #1
      \property Voice.Beam \set #'dir-function = #beam-dir-majority
      [d8_"up" a]
      \property Voice.Beam \set #'dir-function = #beam-dir-mean
      [d_"down" a] 
      \property Voice.Beam \set #'dir-function = #beam-dir-median
      [d_"down" a]
      
      \property Voice.Beam \set #'dir-function = #beam-dir-majority
      \time 3/8
      [d8_"up" a a]
      \property Voice.Beam \set #'dir-function = #beam-dir-mean
      [d_"up" a a] 
      \property Voice.Beam \set #'dir-function = #beam-dir-median
      [d_"down" a a] 

    }
  >
}

%% Local variables:
%% LilyPond-indent-level:2
%% End:
