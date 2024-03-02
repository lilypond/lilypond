\version "2.25.14"

\header {
  texidoc = "Dashed and dotted bar lines print nicely with small values of
@code{set-global-staff-size} and @code{layout-set-staff-size}."
}

%% TODO obviously dashed/dotted bar lines need to be improved, currently they
%% do not print nicely

#(set-global-staff-size 10)

mus =
  { b4 \bar "!" b \bar ";" }

staffGroup =
  \new StaffGroup
  <<
    \new Staff { \clef alto \mus }
    \new TabStaff \mus
  >>

\score {
  \staffGroup
}

\score {
  \staffGroup
  \layout { #(layout-set-staff-size 33) }
}
