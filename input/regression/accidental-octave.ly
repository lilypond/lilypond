
\version "2.2.0"

\header {
texidoc="
This shows how accidentals in different octaves are handled.
(DOCME)
"


}



mel = \notes \transpose c c' {
  \time 4/4 \key d \major
  gis4 g' g gis' | gis2 g' | g1 | gis | g | gis' | g |
  fis4 f' f fis' | fis2 f' | f1 | fis | f | fis' | f |
  \bar "|." \break
}

\score {
  << \context Staff \mel
     \context NoteNames{
	 \set printOctaveNames= ##f
	 \mel
	 }
  >>
}

