\version "2.1.36"
\header {
  texidoc="@cindex Blank Paper

A blank music paper can be produced also by using invisible notes, and removing
@code{Bar_number_engraver}.

" }
\score {
  \notes {
    % \clef violin
    \clef bass 
    \repeat unfold 3 { c1 \break }
  }
  \paper {
    \context {
      \ScoreContext
      \override TimeSignature #'transparent = ##t
      \override NoteHead #'transparent = ##t
      defaultBarType = #""
      \remove Bar_number_engraver
    }
  }
}

