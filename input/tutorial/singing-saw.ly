

accompMotif = \notes \relative c {
	\times 2/3 { c8 g' es' }
	\times 2/3 { c' es, g, }
}

accomp =  \notes \relative c' \autochange Staff {
	\autoBeamOff
	<c2-\arpeggio es g>
	r8 d-.
	\showStaffSwitch
	f,  b,
	\hideStaffSwitch
	\autoBeamOn
	\repeat unfold 2 \accompMotif
}
piano = \context PianoStaff  \notes <
  \context Staff = up <
     s1*2
     \accomp
   >
  \context Staff = down { \clef bass s1*2 }
>

saw = \context Staff \notes  {
	\property Staff.clefOctavation = #7
	r1 b'''2 - \glissando ais'''2 
}

\score {
  <   \saw \piano  >
}
	
