
\version "1.9.4"
\header{
    texidoc="@cindex Bar Number Show All
Second line has bar numbers on start of every measure.
" }

\score{
    \notes\relative c'{
	c1 c c
	\property Score.BarNumber \override
	    #'break-visibility = #end-of-line-invisible
	\break
	c c c
    }
	\paper{ raggedright = ##t }
}


