\version "1.9.2"

\header  {

texidoc = "
Texts may be added to the multi measure rests.

"


}

\score { \notes {
  \time 3/4
  \property Score.skipBars = ##t
  R2._\markup { \center << \musicglyph  #"scripts-dfermata" \roman "Ad lib"  >>  }
  R2.^\fermataMarkup
  R2.^"4"
  R2.*3_\markup { \roman "a1b2c3" }
  R2.*10^"inner"^"top"_"inner"_"bot"
  c'2.
}}
