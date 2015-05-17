

\header
{
    
texidoc = "At line breaks, ottava brackets have no vertical line and
their horizontal line does not stick out.  The dashed line runs until
the end of the line (regardless of prefatory matter).
"

}

\version "2.19.21"


\layout { ragged-right = ##t }
\relative  {
    a''2 b
    \ottava #1
    a2 b \time 3/4  \break c''2. \break
    a
    \ottava #0
    g,,2 b4 c2 a4 
}

