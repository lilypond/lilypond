
\version "2.1.26"
\header{
    texidoc="@cindex Bar Number Show All
Second line has bar numbers on start of every measure.
" }

\score{
    \notes\relative c'{
	c1 c c
	\override Score.BarNumber 
	    #'break-visibility = #end-of-line-invisible
	\break
	c c c
    }
	\paper{ raggedright = ##t }
}


