\version "1.3.148"


\header {
texidoc = "Braces at different sizes don't attach exactly to the staffs.
Previously, some braces were too small, while others were too big; this was
apparently caused by dvips + mf rounding.  Now, braces are just a bit
too big."
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
