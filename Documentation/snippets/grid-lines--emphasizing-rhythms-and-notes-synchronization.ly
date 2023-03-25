%% DO NOT EDIT this file manually; it was automatically
%% generated from the LilyPond Snippet Repository
%% (http://lsr.di.unimi.it).
%%
%% Make any changes in the LSR itself, or in
%% `Documentation/snippets/new/`, then run
%% `scripts/auxiliar/makelsr.pl`.
%%
%% This file is in the public domain.

\version "2.25.3"

\header {
  lsrtags = "editorial-annotations"

  texidoc = "
Regular vertical lines can be drawn between staves to show note
synchronization; however, in case of monophonic music, you may want to
make the second stave invisible, and make the lines shorter like in
this snippet.
"

  doctitle = "Grid lines: emphasizing rhythms and notes synchronization"
} % begin verbatim


\score {
  \new ChoirStaff {
    \relative c'' <<
      \new Staff {
        \time 12/8
        \stemUp
        c4. d8 e8 f g4 f8 e8. d16 c8
      }
      \new Staff {
        % hides staff and notes so that only the grid lines are visible
        \hideNotes
        \hide Staff.BarLine
        \override Staff.StaffSymbol.line-count = #0
        \hide Staff.TimeSignature
        \hide Staff.Clef

        % dummy notes to force regular note spacing
        \once  \override Score.GridLine.thickness = #4.0
        c8 c c
        \once  \override Score.GridLine.thickness = #3.0
        c8 c c
        \once  \override Score.GridLine.thickness = #4.0
        c8 c c
        \once  \override Score.GridLine.thickness = #3.0
        c8 c c
      }
    >>
  }
  \layout {
    \context {
      \Score
      \consists "Grid_line_span_engraver"
      % center grid lines horizontally below note heads
      \override NoteColumn.X-offset = #-0.5
    }
    \context {
      \Staff
      \consists "Grid_point_engraver"
      gridInterval = \musicLength 8
      % set line length and positioning:
      % two staff spaces above center line on hidden staff
      % to four spaces below center line on visible staff
      \override GridPoint.Y-extent = #'(2 . -4)
    }
    ragged-right = ##t
  }
}
