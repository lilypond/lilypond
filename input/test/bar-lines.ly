#(ly:set-option 'old-relative)
\version "1.9.1"
\header {
    texidoc = "@cindex Bar Lines
Different types of bar lines demonstrated.
" }

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
} 
\paper{raggedright = ##t}
}

