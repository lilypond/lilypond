\version "1.7.6"
\header {

texidoc = "Cautionary accidentals are indicated using either
smaller accidentals (default) or parentheses.

"

}

\score {
    \notes {
    c''4
    cis''?4
    \property Staff.Accidental \override #'style = #'parentheses
    cis''?4
    }}
%% new-chords-done %%