
\version "1.9.8"

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

	\context Score \applyoutput #(outputproperty-compatibility (make-type-checker 'paper-column-interface) 'between-system-string "(pagebreak)\n\n")
	\break

c1 }

}

