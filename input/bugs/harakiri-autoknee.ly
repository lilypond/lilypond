
% 
% Autoknee-ing triggers hara-kiri too early. 
% 

\score { \notes \transpose c'''
 \context PianoStaff <
   \context Staff = up { c4 c c c \break c c c c }
   \context Staff = mid { c4 c c c \break s1  }
   \context Staff = down { c8 \translator Staff=mid c \translator
Staff=down c c c4 c c c c c }
 >
 \paper {
  \translator {
   \HaraKiriStaffContext
%   Beam \revert #'auto-knee-gap 
  }
 }
}

