\header {
    
    texidoc ="Broken matter at the end of line does not upset the
    space following rests and notes."  }

\score
{
\notes \relative c' {
    c2.. r8
    c2.. r8
    \time 3/4 \break
    e2 e4 | e2 e4 \time 4/4 \break
}
\paper {
    raggedright = ##t }  
}
