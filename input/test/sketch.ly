\version "1.5.68"
\header {
texidoc="sketch output supported features"
}
\score {
  \notes\relative c''' {
% doesn't work yet  
%    \time 3/4
    a4( a a a )a
    \stemDown
    a,8( b c )d
    \stemUp
    \slurDown d16( c b )a
  }
}
