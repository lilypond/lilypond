\version "1.7.16"
\header {
    texidoc = "Scripts can be stacked. The order is determined by a
priority field, but when objects have the same priority, the input
order determines the order. Objects specified first are closest to the note.
"
}

\score{ \notes { c4^"inner up"^"outer up"_"inner down"_"outer down" }
	\paper { raggedright = ##t}
    } 
%% new-chords-done %%
