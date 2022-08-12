\version "2.23.12"

\header {
  texidoc = "Highlights can be used in contexts at higher level
than @code{Staff}."
}

musI = {
  \staffHighlight lightsteelblue
  c'4 g' g' c''
  c' g' g' <e'' c'''>
  \staffHighlight palegoldenrod
  b f' f' d''
  b f' f' <f'' d'''>
  \staffHighlight lightsteelblue
  c' g' g' c''
  c' g' g' <e'' c'''>
}

\new StaffGroup <<
  \new PianoStaff \with {
    \consists Staff_highlight_engraver
  }
  <<
    \new Staff \with { \remove Staff_highlight_engraver }
      \musI
    \new Staff \with { \remove Staff_highlight_engraver }
      \transpose c' c { \clef bass \musI }
  >>
  \new Staff { g'1~ 1 g'8 fis' g'2.~ 1 g'1~ 1 }
>>
