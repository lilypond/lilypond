\version "1.7.28"

\header {

    texidoc = "Using other fonts can be done by setting font-name for
the appropriate object. This may include Postscript fonts that are
available through (La)TeX.
"


}

\score {
    \notes {
	\property Staff.TimeSignature 	\set #'font-name = #"cmr17"
	\property Score.skipBars = ##t 
	\property Staff.MultiMeasureRestText \set  #'font-name = #"putri8r"
	c'1_\markup { \override #'(font-name . "ptmb8r")
			  { This text is bold Times Roman } }

	     R1*21^"Wait for Utopia Italic"
	     }
    
    \paper { raggedright = ##t }

}
