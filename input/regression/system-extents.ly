\version "2.3.17"
\header {

    texidoc = "The size of every system is correctly determined; this 
    includes postscript constructs such as slurs."

    }
	  

\paper { 
    \context {
	\Score
	\override System #'print-function = #box-grob-stencil
    }
}

\new Voice
{
    \unset Staff.minimumVerticalExtent 
    \slurDown c4 ( g4  c''4)
}

