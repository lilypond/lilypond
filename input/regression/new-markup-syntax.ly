\header  {
texidoc = "New markup syntax."
}

\version "1.7.8"


\score {
  \notes
   {
    \property Voice.TextScript \set #'molecule-callback = #brew-new-markup-molecule
    f'-\markup { foo
		\raise #0.2 \bold bar
		\override #'(baseline-skip . 4) \column < baz bazr bla >
		\hspace #2.0
		\override #'(font-family . music) {
			\lookup #"noteheads-0"
			\char #53
		}

		\combine "X" "+"   
		\combine "o" "/"
		"$\\emptyset$"
		\italic Norsk
		\dynamic sfzp
	}	
    c''4
    }
}
