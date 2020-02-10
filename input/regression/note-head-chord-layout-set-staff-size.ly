\version "2.21.0"

\header {

  texidoc = "Note heads are placed on the correct side of the stem;
this placement changed is not changed by magic values of
layout-set-staff-size. (Fix of issue 5303.)"

}

\layout {
  #(layout-set-staff-size 19)
}


{ <d' f' a'>2 \clef bass <f, a, c> }

