\version "2.6.0"
\header {

    texidoc = "The size of every system is correctly determined; this 
    includes postscript constructs such as slurs."

    }
	  

\layout { 
    \context {
	\Score
	\override System #'print-function = #box-grob-stencil
    }
}

\new Voice
{
    \unset Staff.minimumVerticalExtent
    \override Score.RehearsalMark #'font-size = #20
    
    \slurDown c4 ( g4  c''4)
}

