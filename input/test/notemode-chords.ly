\version "1.7.6"


\score{
	\notes \transpose c c'{
% 1.1.53: @x@ syntax dropped
% if you want fingering, write chord explicitily eg << c-1 e-2 g-3 >>1
%		@c1@ @c-7@ @c-7^5@-1-3
		\chords { c1 c:7 c:7^5 }
	}

}
%% new-chords-done %%
