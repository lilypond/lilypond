
\version "2.4.0"

\header{
texidoc="No auto beams will be put over (manual) repeat bars."
}

\score {
     {
        \time 3/4
        a'4 b' c''8 \bar ":|:" d''8
    }
    \layout{
   raggedright = ##t
}
}
