\version "1.3.146"

\header {
texidoc ="Broken volta spanners behave correctly at left edge in all cases."

}

%{


I recently discovered that although the following patch seemed to fix
the original complaint that it causes other problems (the following
tests were done in 1.3.116.

I have attached a file which has several break-volta interactions.

With the test file and unmodified 1.3.116 I see the following:

Bar 3 - 1st volta spanner centered on first note - prefer it to start
	closer to key signature (can live with this)
Bar 6 - 1st volta continuation - perfect
Bar 9 - 2nd volta spanner starts on left edge of key signature - prefer
	it to start right of key signature
Bar 12 	1st volta starts between first two slurred notes in measure -
	this is not acceptable
Bar 17	like above with hairpin.
Bar 23	like above (I had expected this to be OK)
Bar 20	2nd volta continuation perfect

With the test file and 1.3.116 modified as below I see the following:

Bar 3	Perfect
Bar 6	1st volta continuation starts with staff - oops
Bar 9  	2nd volta starts on papers left margin - OOPS!
Bar 12	Perfect
Bar 17 	Perfect
Bar 20	2nd volta continuation starts with staff - oops
Bar 23	Perfect

%}

voiceB = \notes {
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
voiceC = \notes {
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
voiceD = \notes {
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
voiceE = \notes {
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
