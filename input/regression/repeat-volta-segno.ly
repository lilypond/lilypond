\version "2.18.0"

\header {
  texidoc="
  The segno sign should be automatically combined with the
  appropriate repeat bar line when @code{\inStaffSegno} is
  used.
"
}

\relative c' {
  c1
  \inStaffSegno
  c2^"no repeat" c c c
  \repeat volta 2 {
    \inStaffSegno  % start repeat
  c2^"start repeat" c c c
  }
  \break
  c1
  \repeat volta 2 {
  c2 c c c^"end repeat"
    \inStaffSegno  % end repeat
  }
  c2 c c c
  \repeat volta 2 {
  c2 c c c
  }
  \inStaffSegno  % double repeat
  \repeat volta 2 {
  c2^"double repeat" c c c
  }
}
