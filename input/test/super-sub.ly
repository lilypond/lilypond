\version "1.5.68"
\header {
texidoc="Test super/sub, raise and overstrike."
}



\score {
  \notes \relative a'' {
    c^#'("bar")
    c^#'(("bar"))
    c^#'((("bar")))
    c^#'(bold "bar")
    c^#'((bold) "bar")
    c^#'(((bold)) "bar")
    c^#'(bold ("bar"))
    c^#'(bold "bar")
    c^#'(columns "foe" ((raise . 3) "bar"))
    c^#'(columns "foe" (((raise . 3) "bar")))
    c^#'(columns "foe" (((raise . 3)) "bar"))
    c^#'(columns "foe" (super "12") (sub "3 4"))
    c^#'(columns "foe" (super (overstrike "o") "/") (sub "x"))
    c^#'(columns "foe" (overstrike "o") "/")
    c^#'(columns "foe" ((bold roman overstrike) "o") "/")
    c^#'(columns "foe" ((extent . (0 . 0)) "o") "/")
    c^#'(columns "foo" (super "bar" (super "baz")))
    c
    %% Hmm
    c^#`(columns (lines "" ";" "") (lines "1" ((bold) "2") "3"))
    c^#`(columns (lines "" ";" "") (lines "1" (columns (bold "2")) "3"))
  }
}