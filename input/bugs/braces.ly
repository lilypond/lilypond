\header {
texidoc="Run this through ly2dvi to show differerent brace sizes"
}

\include "paper11.ly"
\paper { linewidth = -1.0 }

\score {
  \context GrandStaff <
    \context Staff = a \notes c''1
    \context Staff = b \notes c''1
  >
}


\include "paper13.ly"
\paper { linewidth = -1.0 }

\score {
  \context GrandStaff <
    \context Staff = a \notes c''1
    \context Staff = b \notes c''1
  >
  \paper {
    \translator {
      \GrandStaffContext
      SystemStartDelimiter \override #'weird = #-5
    }
  }
}


\include "paper16.ly"
\paper { linewidth = -1.0 }

\score {
  \context GrandStaff <
    \context Staff = a \notes c''1
    \context Staff = b \notes c''1
  >
  \paper {
    \translator {
      \GrandStaffContext
      SystemStartDelimiter \override #'weird = #5
    }
  }
}


\include "paper20.ly"
\paper { linewidth = -1.0 }

\score {
  \context GrandStaff <
    \context Staff = a \notes c''1
    \context Staff = b \notes c''1
  >
  \paper {
    \translator {
      \GrandStaffContext
      SystemStartDelimiter \override #'weird = #-7
    }
  }
}


\include "paper26.ly"
\paper { linewidth = -1.0 }

\score {
  \context GrandStaff <
    \context Staff = a \notes c''1
    \context Staff = b \notes c''1
    \context Staff = c \notes c''1
    \context Staff = d \notes c''1
  >
}
