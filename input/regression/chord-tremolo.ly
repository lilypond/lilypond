\version "2.16.0"

\header{
texidoc="
Chord tremolos look like beams, but are a kind of repeat symbol.
To avoid confusion, chord tremolo beams do not reach the stems, but 
leave a gap.  Chord tremolo beams on half notes are not ambiguous,
as half notes cannot appear in a regular beam, and should reach the 
stems.

In this example, each tremolo lasts exactly one measure.

(To ensure that the spacing engine is not confused we add some regular
notes as well.)

"
}

\context Voice \relative c' {
  \time 4/4
  \repeat "tremolo" 16 { d32 e }
  \repeat "tremolo" 8 { d16 e }
  \repeat "tremolo" 4 { d8 e }

  \time 3/4
  \repeat "tremolo" 12 { d32 e }
  \repeat "tremolo" 6 { d16 e } 
  \repeat "tremolo" 3 { d8 e } 

  \time 2/4
  \repeat "tremolo" 8 { d32 e }
  \repeat "tremolo" 4 { d16 e }
  \repeat "tremolo" 2 { d8 e }

  \time 1/4
  \repeat "tremolo" 4 { d32 e }
  \repeat "tremolo" 2 { d16 e }

  c4 c4 c4 c4 c4 
}

