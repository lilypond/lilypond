
\version "2.3.16"

\header {

    texidoc ="Accidentals are placed as closely as possible.
Accidentals in corresponding octaves are aligned.
The top accidental should be nearest to the chord. The
flats in a sixth should be staggered.  "
    
}


\score {
     \context Voice \relative c'
    {
	cis4
	c4
	\transpose c c' {
	    <ges es'>
	    <bis es gis>4
	    <es! as!>
	    <gis! cis!>
	    <g! des'>
	    <ges! es'!>
	}    
	<cis d es fis gis ases bes ces d e! >4
	<bes'! fis!>     
	<bes! cis!>
	<c! es ges beses>
	<bes! c d f >    
	<bes,! c d f >
    }
    \paper {
	raggedright = ##t
    }

}


