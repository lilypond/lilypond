\version "2.3.2"
\header {

    texidoc = "The size of every system is correctly determined; this 
    includes postscript constructs such as slurs."

    }
	  
\score { \notes
\context Voice
{ \unset Staff.minimumVerticalExtent 
    \slurDown c4 ( g4  c''4)

}

    \paper { 
	 \context {
	     \Score
	     \override System #'print-function = #box-grob-stencil
	     }
    }
}

