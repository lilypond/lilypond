\version "1.3.146"

\score{
	\notes \relative c''{
		\slurUp c()a d()g,\break
		\slurDown c()a d()g,\break
		\slurUp a()c d()g,\break
		\slurDown a()c d()g,\break
		\slurDown a()c d()g,\break
	}
	\paper{
		indent = 0.0
		linewidth = 60.0\mm
	}
}
