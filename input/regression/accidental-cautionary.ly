\version "1.5.68"
\header {

texidoc = "Cautionary accidentals are indicated using either
smaller accidentals (default) or parentheses."

}

\score { \notes {
    c''4
    cis''?4
    \property Staff.Accidental \override #'style = #'parentheses
    cis''?4
    }}
