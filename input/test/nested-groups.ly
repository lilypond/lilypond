\header{
texidoc="
In InnerStaffGroup and InnerChoirStaff, the brackets should be shiftet leftwards.
"
filename = 	 "nested-groups.ly"
description = 	 "Test of nested staff groups and choirstaffs"
enteredby = 	 "RZ"
copyright = 	 "public domain"
Tested = 	 "Nested StaffGroups"
}

\version "1.3.146" % rz1

\score { \notes
<
  \context StaffGroup = ga < 
  \context Staff = sb {c' d' e' f'}
  \context InnerStaffGroup = isga <
   \context Staff = sd {c' d' e' f'}
   \context GrandStaff=gs <
     \context Staff = sgsa {c' d' e' f'}
     \context Staff = sgsb {c' d' e' f'}
   >
  \context Staff = sc {c' d' e' f'}
  >
  \context ChoirStaff = csa <
   \context Staff = sd {c' d' e' f'}
    \context InnerStaffGroup=gc <
     \context Staff = sq {c' d' e' f'}
    >
   \context Staff = se {c' d' e' f'}
  >
  >
  \context ChoirStaff = csb < 
   \context Staff = sg {c' d' e' f'}
   \context InnerStaffGroup = isgb <
    \context Staff = sh {c' d' e' f'}
    \context Staff = si {c' d' e' f'}
   >
   \context Staff = sj {c' d' e' f'}
  >

>

 \paper { linewidth = -1 }
}
