\version "2.19.21"
\header {

    texidoc = "If dotted note heads must remain on the left side,
collision resolution moves the dots to the right."

}

\layout { ragged-right = ##t }

\relative {
  \clef bass
  \override Staff.NoteCollision.prefer-dotted-right = ##t
  << <b, g' >4 ^"prefer-dotted-right = #t" \\ { c8. d16 } >>
  << <b g' >4 \\ { d8. d16 } >>
  << <b g' >4 \\ { f'8. d16 } >>
  << <c a' >4 \\ { g'8. d16 } >>
  \override Staff.NoteCollision.prefer-dotted-right = ##f
  << <b g' >4 ^"prefer-dotted-right = #f" \\ { c8. d16 } >>
  << <b g' >4 \\ { d8. d16 } >>
  << <b g' >4 \\ { f'8. d16 } >>
  << <c a' >4 \\ { g'8. d16 } >>
}
