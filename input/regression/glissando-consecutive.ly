\version "2.19.21"
%% This should really be 2.15.0, because the glissando code
%% hasn't been backported -- this is a git-only test at this point

\header {

  texidoc = "Lilypond prints consecutive glissandi."

}

\relative {
  c'1 \glissando d1 \glissando e1
}
