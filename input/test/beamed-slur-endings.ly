\version "1.7.18"
% this looks like an example of slurs, not beaming.  But we
% already have lots of examples of slurs.  candidate for
% future remove-ing.
% possible rename to slur-something.
\score{
	\notes \relative c''{
		\slurUp c8-(-( c-)-( c4-)-)
		\slurDown a8-(-(a-)a4-)
		\break

		\slurDown a8-(-(a-)-(a4-)-)
		\slurUp c8-(-( c-)c4-)
		\break

		\slurDown e8-(-(e-)-( e4-)-)
		\slurUp f,8-(-( f-) f4-)
		\break

		\slurUp e8-(-(e-)-( e4-)-)
		\slurDown f'8-(-(f-)f4-)
		\break

	}
	\paper{
		indent = 0.0
		linewidth = 60.0\mm
	}
}
%% new-chords-done %%
