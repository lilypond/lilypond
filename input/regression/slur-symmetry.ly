\version "1.7.18"

\score{
	\notes\relative c'<
		\time 6/8
		\context Staff{
			e8(e e-) e(d e-) e(c e-) e(b e-)
		}
		\context Staff=x{
			f'8(f f-) f(g f-) f(a f-) f(b f-)
		}
	>
	\paper{
		raggedright = ##t
	}
}
%% new-chords-done %%
