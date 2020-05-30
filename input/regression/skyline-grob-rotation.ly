\version "2.21.0"

\header {
  texidoc = "Skylines reflect grob rotation."
}

#(ly:set-option 'debug-skylines #t)

\layout { indent = 0 }

\new PianoStaff <<
  \new Staff \relative {
    \override TextScript.rotation = #'(30 0 0)
    g4\<^\markup \ellipse "rot. ellipse" e' d'^\markup \box "rotated box" f\!
    \override Hairpin.rotation = #'(15 -1 0)
    g,,4\<^\markup \rounded-box "rounded box" e' d'^"no box" f\!
  }
  \new Staff \relative {
    \override Staff.Clef.rotation = #'(180 0 -0.036)
    <cis' ais'>4 c' g c
    \override Accidental.rotation = #'(30 0 0)
    <cis, ais'>2 e'4 e'
  }
>>
