\version "2.1.26"
\header {
  texidoc="@cindex Blank Paper

Blank music paper with clefs. Change the repeat count to get more staves.

" }
\score {
  \notes {
    % \clef violin
    \clef bass 
    \repeat unfold 3 { c1 \break }
  }
  \paper {
    \translator {
      \ScoreContext
      \override TimeSignature #'transparent = ##t
      \override NoteHead #'transparent = ##t
      defaultBarType = #""
      \remove Bar_number_engraver
    }
  }
}

