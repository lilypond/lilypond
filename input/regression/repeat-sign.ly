\version "2.19.21"
\header {
  texidoc = "The two dots of a repeat sign should be symmetric
to the staff center and avoid staff lines even for exotic staves."
}


mus = \context Voice { \relative { d' e f g \bar ":|." } }

\new Staff {
  <<
    \mus
    \context Voice { s1^"standard staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol.line-positions = #'(-6 -4 -2 0 2)
} {
  <<
    \clef french
    \mus
    \context Voice { s1^"excentric staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol.line-count = #4
} {
  <<
    \mus
    \context Voice { s1^"standard four-line staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol.line-positions = #'(-2 0 2 4)
} {
  <<
    \mus
    \context Voice { s1^"excentric four-line staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol.line-positions = #'(-8 -4)
} {
  <<
    \clef french
    \mus
    \context Voice { s1^"very excentric staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol.line-positions = #'(-2 0)
  \override StaffSymbol.staff-space = #1.5
} {
  <<
    \mus
    \context Voice { s1^"widened by staff-space" }
  >>
}

\new Staff \with {
  \override StaffSymbol.line-positions = #'(-2 0)
} {
  <<
    \mus
    \context Voice { s1^"dots outside" }
  >>
}

\new Staff \with {
  \override StaffSymbol.line-positions = #'(-2.9 -2)
} {
  <<
    \mus
    \context Voice { s1^"narrow staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol.line-positions = #'(-4 -3 -2)
} {
  <<
    \mus
    \context Voice { s1^"dense staff" }
  >>
}

\new Staff \with {
  \override StaffSymbol.line-positions = #'(-6 -5 -2 0 3 5)
} {
  <<
    \mus
    \context Voice { s1^"irregular staff, standard spacing" }
  >>
}

\new Staff \with {
  \override StaffSymbol.line-positions = #'(-4 -2 -1)
  \override StaffSymbol.staff-space = #1.5
} {
  <<
    \mus
    \context Voice { s1^"irregular staff, nonstandard spacing" }
  >>
}

\new Staff \with {
  \override StaffSymbol.line-positions = #'(-10 -2 6 14)
} {
  <<
    \mus
    \context Voice { s1^"dots in outer spaces" }
  >>
}

\new Staff \with {
  \override StaffSymbol.line-positions = #'(-5 -4 -2 2 4 6)
} {
  <<
    \mus
    \context Voice { s1^"dots in the middle" }
  >>
}

\new Staff \with {
  \override StaffSymbol.thickness = #4
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
  \override StaffSymbol.line-positions = #'(-2)
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
