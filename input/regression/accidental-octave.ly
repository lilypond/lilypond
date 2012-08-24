
\version "2.16.0"

\header {
  texidoc="
This shows how accidentals in different octaves are handled. The note names 
are also automatically printed but the octavation has been dropped out.
"


}



mel =  \transpose c c' {
  \time 4/4 \key d \major
  gis4 g' g gis' | gis2 g' | g1 | gis | g | gis' | g |
  fis4 f' f fis' | fis2 f' | f1 | fis | f | fis' | f |
  \bar "|." \break
}

<< \context Staff \mel
   \context NoteNames{
     \set printOctaveNames= ##f
     \mel
   }
 >>
