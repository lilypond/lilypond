% DOCUMENTME!

\version "1.3.146"




voiceE =  \notes {
\clef bass
 \property Staff.instrument = "Bass"
 \property Staff.instr = "B"
% \property Staff.VoltaBracket = \turnOff

 \time 4/4  f,2 (   ) f,8    r8   f8    e8    
\repeat  volta 2
{
 d8.    d16    e8.    f16    f8    c8    c16    c8. 
}
\alternative
{
    {   f,2 (   ) f,8    r8   f8    e8 ( \break   }
    {   ) f,2.    r8   c16    c16      |
    }
}
}
voicedefault =  \notes {
 
 \time 4/4 \key f \major  
 \tempo 4 = 200
}
\score{
        \notes <


        \context Staff="4"
        {
            \voicedefault
            \voiceE 
        }

    >
        \paper {
            \translator {
                 \StaffContext
		 TimeSignature \override   #'style = #'C
            }
        }
}

