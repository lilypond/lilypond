\version "2.1.28"
\header{
texidoc="
Staff margins are also markings attached to barlines.  They should be
left of the staff, and be centered vertically with respect to the staff.  
They may be on normal staves, but also on compound staves, like the 
PianoStaff.
"
}

	


\score {

  \notes \context PianoStaff <<
    \context Staff = treble    {
      \set PianoStaff.instrument = "Piano "
      \set Staff.instrument = "Right " { c''4 }}
    \context Staff = bass { \set Staff.instrument = "Left " \clef bass c4 }>>

\paper {
raggedright = ##t
}}


