\version "1.7.18"

\header {
  texidoc ="Test font selection and scm text markup
"
}


\score{
  \notes\relative c''{
    \stemUp
    a-"text"
    b-"texta"
    c-\markup \bold "textb"

    d-\markup { "one" "two" "three" } |
    e1-\markup { \column << \bold "one"
    { "and" "there" "is" \override #'(font-family . number) "2" }
	\italic "three" >> }
    e2.
    f4-\markup \teeny \number "3"
    f2.
    b4-\markup { "a" \hspace #-8 \raise #2 "b"  "c" }
    b4-\markup { "1" \raise #-2 \hspace #-1 "2"  "3" }
    d4-\markup { \column <<  "Violoncello" "    e" "Contrabasso" >> }
    d4_\markup {\override #'(baseline-skip . 0.0)
		\override #'(word-space . 1.5)
		\column <<  "Violoncello" "    e" "Contrabasso" >> }

    
    g1-"≈÷ƒ‹«’"
    c,,
    c1 
    
    
  }
  \paper{
				%raggedright = ##t
    fontenc = "T1"
    \translator{
      \ScoreContext
      TextScript \override #'font-family = #'roman
      TextScript \override #'font-shape = #'upright
      TextScript \revert #'no-spacing-rods
      TextScript \override #'direction = #1
    }
  }
}
%%% Local variables:
%%% LilyPond-indent-level:2
%%% End:
%% new-chords-done %%
