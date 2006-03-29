
\version "2.7.39"

\header { texidoc = "

You can combine two parts on the same staff using the part
combiner. For vocal scores (hymns), there is no need to add solo/a2
texts, so they should be switched off.

" }

\score{
	\context Staff <<
		\time 4/4
		\partcombine 
			\relative c'' {
				a4 c4.(g8) a4 |
				g4 e' g(f) | 
				b, a c2
			}
			\relative c'' {
				g4 e4.(d8) c4 |
				g'4 c, e(f) |
				d2 a
			}
	>>
	\layout{
		line-width=140.\mm
		\context {
			\Voice
			printPartCombineTexts = ##f
		}
	}
}


