\version "1.5.68"
\header {

    texidoc = "LilyPond correctly determines the size of every
    system. This includes postscript constructs such as slurs."

    }
	  
\score { \notes
\context Voice
{ \property Staff.minimumVerticalExtent \unset
    \slurDown c4 ( g4 ) c''4

}

    \paper { 
	 \translator {
	     \ScoreContext
	     System \override #'molecule-callback = #box-grob-molecule
	     }
    }
}
