\version "2.7.13"
\header {
    texidoc = "Custodes may be engraved in various styles."
}

\layout {
  \context {
    \Staff
    \consists Custos_engraver
  }
  raggedright = ##t
}



{
  \override Staff.Custos  #'neutral-position = #4

  \override Staff.Custos  #'style = #'hufnagel
  c'1^"hufnagel"
  \break < d' a' f''>1

  \override Staff.Custos  #'style = #'medicaea
  c'1^"medicaea"
  \break < d' a' f''>1

  \override Staff.Custos  #'style = #'vaticana
  c'1^"vaticana"
  \break < d' a' f''>1

  \override Staff.Custos  #'style = #'mensural
  c'1^"mensural"
  \break < d' a' f''>1
}
