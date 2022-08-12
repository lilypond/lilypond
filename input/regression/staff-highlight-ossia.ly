\version "2.23.12"

\header {
  texidoc = "When highlights are used in combination with ossia staves,
a highlight only extends to include the ossia staff if it actually spans
a portion of it, but not if it ends at the start of the ossia or if it
starts at the end of the ossia."
}

\layout {
  \context {
    \Staff
    \remove Staff_highlight_engraver
  }
  \context {
    \Score
    \consists Staff_highlight_engraver
  }
}

{
  \staffHighlight aquamarine
  c'1
  \staffHighlight darksalmon
  <<
    { c'1 }
    \new Staff { c'1 }
  >>
  \staffHighlight aquamarine
  c'1
}

{
  \staffHighlight aquamarine
  c'1
  <<
    { c'1 }
    \new Staff { c'1 }
  >>
  \staffHighlight darksalmon
  c'1
}

{
  \staffHighlight aquamarine
  c'1
  \staffHighlight darksalmon
  <<
    { c'1 }
    \new Staff { c'1 }
  >>
  c'1
}

{
  \staffHighlight aquamarine
  c'1
  <<
    { c'1 }
    \new Staff { c'1 }
  >>
  c'1
}

{
  \staffHighlight darksalmon
  <<
    { c'1 }
    \new Staff { c'1 }
  >>
  \staffHighlight aquamarine
  c'1
}

{
  \staffHighlight aquamarine
  c'1
  \staffHighlight darksalmon
  <<
    { c'1 }
    \new Staff { c'1 }
  >>
}
