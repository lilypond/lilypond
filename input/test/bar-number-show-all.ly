
\version "2.7.32"
\header{
    texidoc="@cindex Bar Number Show All
By default, bar numbers are printed only in the first measure.  This 
setting can be overridden, so that bar numbers on start of every measure.
" }

\score{
    \relative c'{
	c1 c c
	\override Score.BarNumber 
	    #'break-visibility = #end-of-line-invisible
	\break
	c c c
    }
	\layout{ ragged-right = ##t }
}


