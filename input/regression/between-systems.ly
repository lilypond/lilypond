\version "1.7.18"

\header{
texidoc="
By inserting @TeX{} commands between systems, you can force pagebreaks.
"
}


% In reality, you'd use #"\\newpage" instead of "(pagebreak)", of course.

\score {
\notes { c1

	\context Score \outputproperty #(make-type-checker 'paper-column-interface)
		#'between-system-string = #"(pagebreak)\n\n"
	\break

c1 }

}
%% new-chords-done %%
