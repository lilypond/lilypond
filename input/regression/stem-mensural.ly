\version "2.24.2"

\header {
  texidoc = "Mensural stems must have exact center alignment.
  This test was made to inspect pixel-granular misalignment bugs."
}

\layout {
  indent = 0
  ragged-right = ##t
}

#(set-global-staff-size 65)

notes = {
  \transpose c d'' { c4 c2 c8  c16 c16  c1 c\breve c\longa }
  \transpose c c' { c4 c2 c8  c16 c16  c1 c\breve c\longa }
  \break
}

\new Score \with {
  \override SpacingSpanner.packed-spacing = ##t
  \remove Bar_number_engraver
} {
  \new Staff \with {
    \remove Clef_engraver
    \remove Time_signature_engraver
  } {
    \time 8/1
    \new Voice \with {
      \override NoteHead.style = #'mensural
      \override Flag.style = #'mensural
    } {
      \sectionLabel "mensural"
      \notes
    }
    \new Voice \with {
      \override NoteHead.style = #'neomensural
      \override Flag.style = #'neomensural
    } {
      \sectionLabel "neomensural"
      \notes
    }
    \new Voice \with {
      \override NoteHead.style = #'petrucci
    } {
      \sectionLabel "petrucci"
      \notes
    }
    \new Voice \with {
      \override NoteHead.style = #'blackpetrucci
    } {
      \sectionLabel "blackpetrucci"
      \notes
    }
    \new Voice \with {
      \override NoteHead.style = #'semipetrucci
    } {
      \sectionLabel "semipetrucci"
      \notes
    }
  }
}

