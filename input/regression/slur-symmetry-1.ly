#(ly:set-option 'old-relative)
\version "2.1.7"
\header
{
  texidoc = "Symmetric figures should lead to symmetric slurs."
}

\score{
	\notes\relative c''<<
		\time 4/4
		\context Staff{
			f8(f f f) f(g g f) f(a a f) f(b b f)
		}
		\new Staff{
			e,(e e e) e(d d e) e(c c e) e(b b e)
		}
	>>
	\paper{
		raggedright = ##t
	}
}

