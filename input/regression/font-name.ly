\version "1.9.1"

\header {

    texidoc = "Using other fonts can be done by setting font-name for
the appropriate object. This may include Postscript fonts that are
available through (La)TeX.

"


}

%{

 Postscript fonts are switched off by default,
 for compatibility with TeX installations that have no
 PS fonts installed.
%}

\score {
    \notes {
	\property Staff.TimeSignature 	\set #'font-name = #"cmr17"
	\property Score.skipBars = ##t

	% use font-name putri8r for Utopia Italic :
	
	\property Staff.MultiMeasureRestText \set  #'font-name = #"cmss12"


	% use "ptmb8r" for Times Roman
	
	c'1_\markup { \override #'(font-name . "cmdunh10")
			  { This text is Dunhill } }

	     R1*21^"Wait for Utopia Italic"
	     }
    
    \paper { raggedright = ##t }

}
