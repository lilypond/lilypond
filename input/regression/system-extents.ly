\header {

    texidoc = "LilyPond correctly determines the size of every
    system. This includes postscript constructs such as slurs."

    }
	  
\score { \notes
\context Voice
{\slurDown c4 ( g4 ) c''4

}

    \paper { 
	 \translator {
	     \ScoreContext
	     System \override #'molecule-callback = #box-grob-molecule
	     }
    }
}
