\version "1.5.68"

\header{

texidoc =  " Volta braces are hung on barlines. At the start of the
line, they should always start after the prefatory matter.  "

}



voiceB =  \notes {
    \clef bass
     \property Staff.instrument = "Bass"
     \property Staff.instr = "B"
     \key f \minor 
     \time 4/4  
    f,2 (   ) f,8    r8   f8    e8    
    \repeat  volta 2
    {
         d8.    d16    e8.    f16    f8    c8    c16    c8.    \break
    }
    \alternative
    {
        { 	
	    f,2 (   ) f,8    r8   f8    e8 
	    f,2 (   ) f,8    r8   f8    e8 
	    g1 \break   
	    f,2 (   ) f,8    r8   f8    e8 
	    f,2 (   ) f,8    r8   f8    e8 
	    g1 \break   
	}
        {   
	    f,2.    r8   c16    c16      | 
	}
    }
}
voiceC =  \notes {
    \repeat volta 2
    {
     bes8    bes8    c'8    c'8    a4    r8   g16    f16      |
     d'8.    c'16    c'8.    d'16    c'8    c'8    f16    g8.    |
     \break
    }
    \alternative
    {
        {	a2 (   ) a8    r8   a8    bes8    }
        {	a2.    r8   g16    f16      |
     	d'8.   c'16   c'8.   d'16   c'8   c'8   c'16    c'8.    |
        }
    }
}
voiceD =  \notes {
    \repeat volta 2
    {
     bes8    bes8    c'8    c'8    a4    r8   g16    f16      |
     d'8.    c'16    c'8.    d'16    c'8    c'8    f16    g8.    |
     \break
    }
    \alternative
    {
        {	a2 \> a8    r8   a8  \!  bes8    
	}
        {	a2.    r8   g16    f16      |
      	    d'8.   c'16   c'8.   d'16   c'8   c'8   c'16    c'8.    | \break
      	    d'8.   c'16   c'8.   d'16   c'8   c'8   c'16    c'8.    |
        }
    }
}
voiceE =  \notes {
    \repeat volta 2
    {
     bes8    bes8    c'8    c'8    a4    r8   g16    f16      |
     d'8.    c'16    c'8.    d'16    c'8    c'8    f16    g8.    | \break
    }
    \alternative
    {
        {	
	    a2  a8    r8   a8    bes8    

	}
        {	
	    a2.    r8   g16    f16      |
      	    d'8.   c'16   c'8.   d'16   c'8   c'8   c'16    c'8.    | 
        }
    }
}
\score{
        \notes <


	\context Staff="4"
	{
	    \voiceB 
	    \voiceC
	    \break
	    \voiceD
	    \voiceE
	}

    >
	\paper {
	    \translator {
	    	 \StaffContext 
		 \consists Instrument_name_engraver
	    }
	}
}
