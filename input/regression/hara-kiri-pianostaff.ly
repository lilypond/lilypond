\header {
texidoc = "Hara kiri should not upset fixed distance alignment like in pianostaff. In this example the middle staff is harakiried."
}

	
\score { \notes \transpose c'''
 \context PianoStaff <
   \context Staff = up {  c c c c \break }
   \context Staff = mid {  s1 \break }
   \context Staff = down {  c4 c c c \break }
 >
 \paper {
  \translator {
   \HaraKiriStaffContext
  }
 }
}

