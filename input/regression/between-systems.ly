
\header{
texidoc="
The same mechanism can be  used  to force pagebreaks.
";
}


% In reality, you'd use #"\\newpage" instead of "(pagebreak)", of course.

\score {
\notes { c1

	\context Score \outputproperty #(make-type-checker 'paper-column-interface)
		#'between-system-string = #"(pagebreak)\n\n"
	\break

c1 }

}
