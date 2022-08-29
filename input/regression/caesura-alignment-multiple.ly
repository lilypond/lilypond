\version "2.23.13"

\header {
  texidoc="Caesura scripts can align to breath marks in some staves
and to bar lines in others.  The output should have one staff with a
fermata over `railroad tracks'.  The other staves should have a
fermata over a comma at bar lines, and the scripts should align to the
bar lines individually."
}

music = \fixed c' {
  f1
  \caesura \fermata
}

<<
  \new Staff \music

  \new Staff \with {
    caesuraType = #'((scripts . (outsidecomma)))
  } \music

  \new Staff \with {
    caesuraType = #'((scripts . (outsidecomma)))
    \override BarLine.hair-thickness = 40
  } \music
>>
