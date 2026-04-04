\version "2.27.0"

\header {
  texidoc = "Stanza numbers get typeset for each instance of @code{\\stanza}."
}

% This used not to be true with the pre-2.27.0 \set stanza = ... syntax
% since the Stanza_number_engraver checked whether the stanza had been set
% using eq? between the old and the new setting.

\new Staff \relative {
  \repeat unfold 3 { c'4 d e c }
}
\addlyrics {
  \repeat unfold 3 {
    \stanza "Sing:"
    Frè -- re Jac -- ques
  }
}
