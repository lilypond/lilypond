\version "2.17.30"

\header {
  texidoc = "An empty bar line does not confuse the spacing engine too much.
The two scores should look approximately the same."
}

\layout {
  \context {
    \Score
    \accepts "Top"
  }

  \context {
    \name "Top"
    \alias "Staff"
    \type "Engraver_group"
    \consists "Output_property_engraver"
    \consists "Bar_engraver"
    \consists "Axis_group_engraver"

    \hide BarLine
  }
}

\paper {
  ragged-right = ##t
}

\score {
  <<
    \new Top { s1 }
    \new Staff { c''2 c''4 c''4 }
  >>
}

\score {
  { c''2 c''4 c''4 }
}
