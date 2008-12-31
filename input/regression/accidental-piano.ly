\header {

    texidoc = "In piano accidental style, notes in both staves
influence each other.  In this example, each note should have an
accidental."

}

\version "2.12.0"

\layout
{
    ragged-right = ##t
}
    
\new PianoStaff \relative <<
    #(set-accidental-style 'piano)
    \new Staff { ges'4 ges4 }
    \new Staff { r8 gis r8 gis }
    >>
