\header {

    texidoc = "Kneed beams (often happens with cross-staff beams)
should look good when there are multiple beams: all the beams should
go on continuously at the staff change. Stems in both staffs reach up
to the last beam.
"
}

\score { \notes
	 \context PianoStaff \relative c' <
	     \context Staff = SA {
		 \stemDown
		 [c8 c16 \translator Staff = SB \stemUp c16 ]
		 \stemBoth
		 [f g \translator Staff = SA a c] 
			       }
	     \context Staff = SB \relative c' {
\clef bass
		 [b8 b16 \stemUp b,,16 ]
		 [b8 b16 \stemDown b''16 b ]
	     }	     
	     
	     >

}
