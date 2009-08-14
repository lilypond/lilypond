\version "2.12.0"
\include "example-header.ily"


\include "predefined-guitar-fretboards.ly"

%lowerChords = { \override ChordName #'extra-offset = #'( 0.0 . -1.3 ) }

global = {
  \time 4/4
  \key bes \major
  \numericTimeSignature
  #(set-global-staff-size 20)
}

melody = \relative c'' {
  \key d \minor
  \partial 4. 
  b4-. cis8 d4 r4 r8 cis d e  %1
  \times 2/3 {f4 e d} e8 d4.    %2
  g4. bes, ~ bes4    %3
}

harmonies = \chordmode {
  %\lowerChords 
  s4. d1:m | d:7 | g1:m |   % 1-3
}

text = \lyricmode {
  We need words! We need some ly -- rics to go
  with this tune!
}
%\header {
%  title = "Lágrimas negras"
%  composer = "Miguel Matamoros"
%  poet = "arr. Jonathan Kulp"
%  arranger = "1894-1971"
%}
%
\score {
  <<
    \new ChordNames { \harmonies }
    \new FretBoards { \harmonies }
    \new Staff  { 
      \context Voice = "vocal" { \melody }
    }
    \new Lyrics \lyricsto "vocal" \text
  >>
}

