\version "1.7.16"
\header {

texidoc = "Cautionary accidentals are indicated using either
parentheses (default) or smaller accidentals.

"

}

\score {
    \notes {
    c''4
    cis''?4
    \property Staff.Accidental \set #'cautionary-style = #'smaller
    cis''?4
    \property Staff.Accidental \set #'cautionary-style = #'parentheses
    cis''?4
    }}
%% new-chords-done %%
