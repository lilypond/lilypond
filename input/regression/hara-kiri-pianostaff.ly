\version "1.7.19"
\header {
texidoc = "Hara kiri should not upset fixed distance alignment like in pianostaff. In this example the middle staff is harakiried."
}

	
\score { \notes \transpose c c''
 \context PianoStaff <
   \context Staff = up {  c c c c \break }
   \context Staff = mid {  s1 \break }
   \context Staff = down {  c4 c c c \break }
 >
 \paper {
  \translator {
   \RemoveEmptyStaffContext
  }
 }
}


