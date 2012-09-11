\version "2.16.0"
\header {
  texidoc = "The two dots of a repeat sign should be symmetric
to the staff centre and avoid staff lines even for exotic staves."
}


mus = \context Voice { \relative f' { d e f g \bar ":|" } }

\new Staff {
  <<
    \mus
    \context Voice { s1^"standard staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol #'line-positions = #'(-6 -4 -2 0 2)
} {
  <<
    \clef french
    \mus
    \context Voice { s1^"excentric staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol #'line-count = #4
} {
  <<
    \mus
    \context Voice { s1^"standard four-line staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol #'line-positions = #'(-2 0 2 4)
} {
  <<
    \mus
    \context Voice { s1^"excentric four-line staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol #'line-positions = #'(-7 -4)
} {
  <<
    \clef french
    \mus
    \context Voice { s1^"very excentric staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol #'staff-space = #1.5
  \override StaffSymbol #'line-positions = #'(-2 0)
} {
  <<
    \mus
    \context Voice { s1^"as wide as previous" }
  >>
}

\new Staff \with {
  \override StaffSymbol #'line-positions = #'(-2.9 -2)
} {
  <<
    \mus
    \context Voice { s1^"narrow staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol #'line-positions = #'(-4 -3 -2)
} {
  <<
    \mus
    \context Voice { s1^"dense staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol #'line-positions = #'(-4 -3 -2)
  \override StaffSymbol #'staff-space = #0.8
} {
  <<
    \mus
    \context Voice { s1^"denser staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol #'line-positions = #'(-6 -2 0 5)
} {
  <<
    \mus
    \context Voice { s1^"irregular staff, standard spacing" }
  >>
}

\new Staff \with {
  \override StaffSymbol #'line-positions = #'(-4 -2 -1)
  \override StaffSymbol #'staff-space = #1.5
} {
  <<
    \mus
    \context Voice { s1^"irregular staff, nonstandard spacing" }
  >>
}

\new Staff \with {
  \override StaffSymbol #'thickness = #4
} {
  <<
    \clef french
    \mus
    \context Voice {
      s1^"thick-lined staff"
    }
  >>
}

\new Staff \with {
  \override StaffSymbol #'line-positions = #'(-2)
} {
  <<
    \mus
    \context Voice { s1^"single line staff (zero height)" }
  >>
}

\new Staff {
  \stopStaff
  <<
    \mus
    \context Voice { s1^"no staff" }
  >>
}
