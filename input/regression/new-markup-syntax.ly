\header  {
texidoc = "With the new markup syntax, text may be written in various manners."
}

\version "2.5.2"


\score {
  
   {
    f'-\markup {
		foo
		\raise #0.2 \hbracket \bold bar
		\override #'(baseline-skip . 4)

		\bracket \column { baz bazr bla }
		\hspace #2.0
		\override #'(font-family . music) {
			\lookup #"noteheads-0"
			\char #53
		}
		\semiflat

		{ }
		
		\combine "X" "+"   
		\combine "o" "/"
%		\char-number #"abc1234abc"
		\box \column { { "string 1" } { "string 2" } }
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
