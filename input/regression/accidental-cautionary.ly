\version "2.3.4"
\header {

texidoc = "Cautionary accidentals are indicated using either
parentheses (default) or smaller accidentals.

"

}

\paper { raggedright = ##t }

{
    c''4
    cis''?4
    \override Staff.Accidental  #'cautionary-style = #'smaller
    cis''?4
    \override Staff.Accidental  #'cautionary-style = #'parentheses
    cis''?4
}

