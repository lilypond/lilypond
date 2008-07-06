
\header {
  
  texidoc = "With balloon texts, objects in the output can be marked,
with lines and explanatory text added."
  
}
\version "2.11.51"

\layout{ ragged-right = ##t }

\new Voice \with {\consists "Balloon_engraver" }
{
  \relative c'  {
    \balloonGrobText #'Stem #'(3 . 4) \markup { "I'm a Stem" }
    <c-\balloonText #'(-2 . -2) \markup { \simple #"hoi" }  >8
  }
}
