\version "1.5.68"
\header { texidoc = "

A voicelet (a very short voice to get polyphonic chords correct)
should not confuse the spacing engine."
 }


	\score {
\notes {  \context Staff {
  c4
  <
    \context Voice = I \relative c'' { \stemUp r4 dis4 }
    \context Voice = III \relative c'' { \stemUp \shiftOn r4 bis  \shiftOff}
    \context Voice = IV \relative c'' {
      \stemDown
      \shiftOn s4 gis }
    \context Voice =  II \relative c' { \stemDown
       % idem

      r4 fis  }
  >
  c4
}}
\paper { linewidth = -1. }
}
