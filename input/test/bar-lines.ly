
\version "2.1.36"
\header {
    texidoc = "@cindex Bar Lines
There a many types of bar lines available.
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
