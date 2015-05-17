
\version "2.19.21"
\header {

  texidoc = "Spacing uses the duration of the notes, but disregards
    grace notes for this. In this example, the 8ths around the grace
    are spaced exactly as the other 8th notes.

"
}

\layout { ragged-right = ##t}

\relative
\context Voice 
{
  c''8[  c8]
  \grace {  b16 }
  c8[ c8]  c8[ c8] 

}




