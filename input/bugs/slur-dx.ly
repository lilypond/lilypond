\header{
texidoc="
Arg, right ending of slur is too far right.  I did make a better
test .ly for this, but can't seem to find it now.
";
}

\score {
  \notes \relative c'' {
    \property Voice.Stem \set #'direction = #1
    \property Voice.Slur \set #'direction = #1
    d,32( d'4 )d8..
    \property Voice.Slur \set #'attachment = #'(stem . stem)
    d,32( d'4 )d8..
  }
  \paper {
    linewidth = -1.;
  }
} 
