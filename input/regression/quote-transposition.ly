
\header
{

    texidoc = "Quotations take into account the transposition of both  source and target.
In this example, all instruments play sounding central C, the target is a instrument in F."

}
\version "2.1.26"


\addquote clarinet \notes {
    \transposition bes
    d'16 d'16 d'8 
    d'16 d'16 d'8 
    d'16 d'16 d'8 
    d'16 d'16 d'8 
    }
\addquote sax \notes {
    \transposition es'
    a8 a a a a a  a a 
    }

\score {
    \notes{
	\transposition f  % french horn
	
	g'4
	<< \quote clarinet 4 s4^"clar" >> 
	<< \quote sax 4 s4^"sax" >> 
    }
}

