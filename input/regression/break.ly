\version "1.5.68"

\header{
texidoc="
Breaks can be encouraged and discouraged using @code{\\break} and
@code{\\noBreak}.  They are abbrevs for @code{\\penalty} commands.
"
}


\score{
	\notes\context Voice{
	\emptyText
	c1 \noBreak c1 \noBreak \mark "nobreak" c1 \noBreak
	c1 \break \mark "break" c1 \break \mark "break" c1 
	}
	\paper {
	    indent = 0.0
	    linewidth = 4.0\cm}
}
