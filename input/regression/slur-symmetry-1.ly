\version "1.3.146"

\score{
	\notes\relative c''<
		\time 4/4
		\context Staff{
			f8(f f)f f(g g)f f(a a)f f(b b)f
		}
		\context Staff=x{
			e,(e e)e e(d d)e e(c c)e e(b b)e
		}
	>
	\paper{
		linewidth=-1.
	}
}
