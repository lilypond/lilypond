\header{
texidoc="
Breaks can be encouraged and discouraged using @code{\break} and
@code{\nobreak}.  They are abbrevs for @code{\penalty} commands.
";
}


\score{
	\notes\context Voice{
	\emptyText
	c1 c1^"no break after 2nd note"  \nobreak c1 c1
	  
	  c1^"break after this" \break c1 c1 
	}
	\paper { linewidth = 4.0\cm;}
}
