\version "2.1.22"
\header {

    texidoc = "LilyPond correctly determines the size of every
    system. This includes postscript constructs such as slurs."

    }
	  
\score { \notes
\context Voice
{ \unset Staff.minimumVerticalExtent 
    \slurDown c4 ( g4  c''4)

}

    \paper { 
	 \translator {
	     \ScoreContext
	     System \override #'print-function = #box-grob-stencil
	     }
    }
}

