\version "1.5.68"
\header {
  texidoc="Blank music paper with clefs"
}
\score {
  \notes {
    % \clef violin
    \clef bass 
    \repeat unfold 9 { c1 \break }
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
