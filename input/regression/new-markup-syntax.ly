\header  {
texidoc = "New markup syntax."
}

\version "1.7.18"


\score {
  \notes
   {
    \property Voice.TextScript \set #'molecule-callback = #brew-new-markup-molecule
    f'-\markup {
		foo
		\raise #0.2 \hbracket \bold bar
		\override #'(baseline-skip . 4)

		\bracket \column << baz bazr bla >>
		\hspace #2.0
		\override #'(font-family . music) {
			\lookup #"noteheads-0"
			\char #53
		}
		\musicglyph #"accidentals--1"
		\combine "X" "+"   
		\combine "o" "/"

		\box \column << { "string 1" } { "string 2" } >>
		"$\\emptyset$"
		\italic Norsk
		\super "2"
		\dynamic sfzp
		\huge { "A" \smaller "A" \smaller \smaller "A"
			\smaller \smaller \smaller "A" }
		\sub "alike"
	}	
    c''4
    }
}
