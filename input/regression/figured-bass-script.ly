\version "2.25.0"

\header {
  texidoc = "Figured bass in a @code{Staff} context doesn't collide
with articulations."
}

<<
  \new Staff = "myStaff"
  \figuremode {
    <4>4 <10 6>8 s8
    \bassFigureStaffAlignmentDown
    <6 4>4 <6 4>
  }

  \context Staff = "myStaff" {
    \clef bass
    c'4-! c'8-> r8 c,4-> c,-!
  }
>>
