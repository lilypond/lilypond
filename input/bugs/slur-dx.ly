\version "1.5.68"

\header{
texidoc = "
 Wouldbe-steep-starting slurs look ugly.
" }

\score {
  \notes \relative c'' {
    \property Voice.Stem \set #'direction = #1
    \property Voice.Slur \set #'direction = #1
    d,32( d'4 )d8..
    \property Voice.Slur \set #'attachment = #'(stem . stem)
    d,32( d'4 )d8..
  }
  \paper {
    linewidth = -1.
  }
} 
