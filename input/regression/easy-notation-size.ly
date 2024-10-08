\version "2.25.21"

\header {
  texidoc = "Easy noteheads should be scalable in size, like in grace notes.
This works with @code{\magnifyStaff} and @code{layout-set-staff-size} as well."
}

%% Due to issue #6756 the output of \magnifyStaff in combination with \huge
%% resp. \tiny is currently wrong.
%% Better to expose the problem than to hide it --Harm

\layout { ragged-right = ##t }

m = {
  \acciaccatura g'8 c'4 d' e' f' c'1
}

mus = { \m \bar "||" \huge \m \bar "||" \tiny \m \bar "||" }

music =
  <<
    \new Staff \mus
    \new Staff \with \easyHeadsOn \mus
    \new Staff \with { \easyHeadsOn \magnifyStaff #(magstep 5) } \mus
    \new Staff \with { \easyHeadsOn \magnifyStaff #(magstep -5) } \mus
  >>

\score { \music}

\score {
  \music
  \layout {
    #(layout-set-staff-size 30)
  }
}
