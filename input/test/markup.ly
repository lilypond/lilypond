\version "1.5.68"

\header {
  texidoc ="Test font selection and scm text markup"
%% Note that the font-encoding trick only works when uning latex,
%% ie, you must use ly2dvi.

}


\score{
  \notes\relative c''{
    \stemUp
				% {
    a-"text"
    b-#"texta"
    c-#'(bold "textb")

    d-#'(lines "one" "two" "three")
    e-#'(lines (bold "one") 
    (columns "and" "there" "is" ((font-family . number) "2"))
    (italic "three"))
    f-#'(finger "3")
    g-#'(music (named "noteheads-2"
    ((kern . -0.1) "flags-stem")
    (((kern . -0.1) (raise . 3.5)) "flags-u3")))
    b-#'(columns "a" (((kern . 3) (raise . 2)) "b") "c")
    c-#'(columns "1" (((raise . -2) (kern . -1)) "2") "3")
				% }
    d-#'(lines "Violoncello" "    e" "Contrabasso")
    e-#'((lines (baselineskip . 0) (kern . 1.5)) "Violoncello" "    e" "Contrabasso")
    e-#'(((baselineskip . 0) (kern . 1.5) lines) "Violoncello" "    e" "Contrabasso")
    g-"≈÷ƒ‹«’"
    c,,
    c1 c1
    
    \stemUp
    c4^#'(lines "1" "" "2")
    c^#'(lines "3" "4" "5")
    \stemDown
    c_#'(lines "6" "" "7")
    c_#'(lines "8" "9" "0")
    
    \stemUp
    c4^#'(lines "1" "")
    c^#'(lines "2" "3")
    \stemDown
    c_#'(lines "" "4")
    c_#'(lines "5" "6")
    
    c^#'(columns "foe" ((raise . 3) "bar"))
    c^#'(columns "foe" (super "12") (sub "3 4"))
    %% UGHUGH
    c^#'(columns "foe" (super ((raise . 1) "12")) (sub ((raise . -1) "3 4")))
    c^#'(columns "foe" (super (overstrike "o") "/") (sub "x"))
    c^#'(columns "foe" (overstrike "o") "/")
    c^#'(columns "foe" ((bold roman overstrike) "o") "/")
    c^#'(columns "foe" ((extent . (0 . 0)) "o") "/")
    c^#'(columns "foo" (super "bar" (super "baz")))
    c
    %% c^#`(columns (lines "" ";" "") (lines "1" ((bold) "2") "3"))
    c^#`(columns (lines "" ";" "") (lines "1" "2" "3"))
    c^#`(columns (lines "" ";" "") (lines "1" (bold "2") "3"))
    c^#`(columns (lines "" ";" "") (lines "1" (columns (bold "2")) "3"))
	
    
  }
  \paper{
				%linewidth = -1.\mm
    fontenc = "T1"
    \translator{
      \ScoreContext
      TextScript \override #'font-family = #'roman
      TextScript \override #'font-shape = #'upright
      TextScript \revert #'no-spacing-rods
      TextScript \override #'direction = #1
      TextScript \override #'font-encoding = #'T1
    }
  }
}
%%% Local variables:
%%% LilyPond-indent-level:2
%%% End:
