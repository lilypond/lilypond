\version "1.7.18"

\header{
texidoc="
By inserting @TeX{} commands between systems, you can force pagebreaks.

In reality, you'd use the LateX command @code{\\newpage} instead of (pagebreak)
of course. 
"
}


% 

\score {
\notes \relative c' { c1

	\context Score \outputproperty #(make-type-checker 'paper-column-interface)
		#'between-system-string = #"(pagebreak)\n\n"
	\break

c1 }

}

