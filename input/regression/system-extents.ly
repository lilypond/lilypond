\version "2.2.0"
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
	     \ScoreContext
	     \override System #'print-function = #box-grob-stencil
	     }
    }
}

