\version "1.9.8"
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
      TimeSignature \override #'transparent = ##t
      NoteHead \override #'transparent = ##t
      defaultBarType = #""
      \remove Bar_number_engraver
    }
  }
}

