
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

We should see:

  up down down

  up up down
";
}

\score {
  \notes \relative c'' {
    % the default
    %\property Voice.Beam \set #'dir-function = #beam-dir-majority
    [d8 a]
    \property Voice.Beam \set #'dir-function = #beam-dir-mean
    [d a] 
    \property Voice.Beam \set #'dir-function = #beam-dir-median
    [d a]
    
    \property Voice.Beam \set #'dir-function = #beam-dir-majority
    \time 3/8;
    [d8 a a]
    \property Voice.Beam \set #'dir-function = #beam-dir-mean
    [d a a] 
    \property Voice.Beam \set #'dir-function = #beam-dir-median
    [d a a] 
  }
}
