\version "1.3.129"
\include "paper16.ly"
\score{
  \notes \context Staff = pr {
         \context Voice < a b c' e'>
  }
  \paper{
    \translator {
        \StaffContext
        StaffSymbol \override #'staff-space = #1.3
    }
  }
}

