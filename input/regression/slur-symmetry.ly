\version "2.3.16"
\header
{
    texidoc = "Symmetric figures should lead to symmetric slurs."

}

\score{
	\relative c'<<
		\time 6/8
		\context Staff{
			e8(e e) e(d e) e(c e) e(b e)
		}
		\new Staff{
			f'8(f f) f(g f) f(a f) f(b f)
		}
	>>
	\paper{
		raggedright = ##t
	}
}

