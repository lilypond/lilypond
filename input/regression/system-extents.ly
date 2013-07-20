\version "2.17.6"
\header {

    texidoc = "The size of every system is correctly determined; this 
    includes postscript constructs such as slurs."

    }
	  

\layout { 
    \context {
	\Score
	\override System.stencil = #box-grob-stencil
    }
}

\new Voice
{
    \override Score.RehearsalMark.font-size = #20
    
    \slurDown c4 ( g4  c''4)
}

