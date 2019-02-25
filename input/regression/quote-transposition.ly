
\header
{

    texidoc = "Quotations take into account the transposition of both
source and target.  In this example, all instruments play sounding
central C, the target is a instrument in F.  The target part may be
@code{\\transpose}d.  The quoted pitches will stay unchanged."

}

\version "2.21.0"

\layout { ragged-right = ##t }


\addQuote clarinet  {
    \transposition bes
    d'16 d'16 d'8 
    d'16 d'16 d'8 
    d'16 d'16 d'8 
    d'16 d'16 d'8 
}

\addQuote sax  {
    \transposition es'
    a8 a a a a a  a a 
}

quoteTest = {
    \transposition f  % french horn
    
    g'4
    << \quoteDuring "clarinet" { \skip 4 } s4^"clar" >> 
    << \quoteDuring "sax" { \skip 4 } s4^"sax" >> 
}


<< \quoteTest
   \new Staff
   << \transpose c' d' \quoteTest
     s4_"up 1 tone"
  >>
>>
