\version "2.25.23"

\header {
  texidoc = "LilyPond supports mensural music (including Kievan square
notation) by providing special contexts.

This test shows basic note values but no ligatures."
}

music = { e'8 d' b a e'4 d' e'2 d' e'1 d' eis'\breve d' ees'\longa d' }
musicx = { e'16 d' b a \music }
musicxx = { e'32 d' b a \musicx }

\new KievanStaff { e'8[ d'] b[ a] e'2. d'4. \music }
\new PetrucciStaff { \clef "petrucci-c3" \musicx }
\new MensuralStaff { \clef "mensural-c3" \musicxx }
