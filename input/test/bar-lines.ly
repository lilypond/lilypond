
\version "2.1.26"
\header {
    texidoc = "@cindex Bar Lines
Different types of bar lines demonstrated.
" }
% TODO: dashed "|", HJJ
\score
{
\notes \relative c'' {
c4
\bar "|." c
\bar "|:"c
\bar "||"c
\bar ":|"  c
\bar ".|" c
\bar ".|." c
\bar "|"  c
\bar ":" c
} 
\paper{raggedright = ##t}
}
