
\score {
\notes { c1

	\context Score \outputproperty #(make-type-checker 'paper-column-interface)
		#'between-system-string = #"(pagebreak)\n\n"
	\break

c1 }

}
