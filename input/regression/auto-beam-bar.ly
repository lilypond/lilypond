
\version "1.9.7"

\header{
texidoc="No auto beams will be put over (manual) repeat bars."
}

\score {
    \notes {
        \time 3/4
        a'4 b' c''8 \bar ":|:" d''8
    }
    \paper{
   raggedright = ##t
}
}
