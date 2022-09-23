\version "2.19.16"
\header {
  texidoc = "The two dots of a repeat sign should be symmetric
to the staff center and avoid staff lines even for exotic staves.
Test layout-set-staff-size."
}

\score {
  <<
    \context Staff = "s1" \with {
      \override StaffSymbol.staff-space = #0.5
    } {
      s1 \bar ":|."
    }

    \context Staff = "s2" \with {
      \override StaffSymbol.staff-space = #0.6
    } {
      s1 \bar ":|."
    }

    \context Staff = "s3" {
      s1 \bar ":|."
    }
  >>

  \layout {
    #(layout-set-staff-size 10)
  }
}

\score
{
  <<
    \context Staff = "s1" \with {
      \override StaffSymbol.staff-space = #0.5
    } {
      s1 \bar ":|."
    }

    \context Staff = "s2" \with {
      \override StaffSymbol.staff-space = #0.6
    } {
      s1 \bar ":|."
    }

    \context Staff = "s3" {
      s1 \bar ":|."
    }
  >>
}
