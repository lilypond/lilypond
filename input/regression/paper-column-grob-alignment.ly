\version "2.25.11"

#(set-global-staff-size 30)

\header {
  texidoc = "Objects like articulations, lyrics, dynamics, etc., are
aligned correctly even when they aren't attached directly to notes.

An object's parent may be a @code{PaperColumn} (instead of a more usual
@code{NoteHead}); this can happen, for example, when a
@code{DynamicText} grob is attached to a spacer rest, or when a
@code{Lyrics} context doesn't have the @code{associatedVoice} property
set.  In that case, LilyPond should find note heads belonging to this
@code{PaperColumn} and align the object on these note heads.  If there
are no note heads in the @code{PaperColumn}, the object are aligned
using a placeholder extent to ensure consistent spacing between objects
attached to @code{PaperColumns} and @code{NoteHeads} grobs.

Note that the placeholder extent is not used if there are any note
heads in the respective @code{PaperColumn} grob, even if they have
empty stencils.

In the test cases below the @code{PaperColumn} grob location has been
marked with a blue line."
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% stuff for displaying PaperColumns and placeholder extent

\layout {
  \context {
    \Voice
    \consists "Grid_point_engraver"
    gridInterval = #(ly:make-moment 1/4)
    \override GridPoint.Y-extent = #'(-1 . 3)
  }
  \context {
    \Staff
    \consists "Grid_line_span_engraver"
    \override GridLine.color = #blue
    \override GridLine.layer = #2
  }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% actual tests

\layout {
  ragged-right = ##f
}

\markup { Distances between dynamics should be identical: }
\score {
  \new Staff <<
    \new Voice { a'2 a' }
    \new Voice {
      s4\p
      s\p
      s\p
      s\p
    }
  >>
  \layout {
    line-width = 7.5\cm
  }
}

\markup { Dynamics should be centered on note columns: }
\score {
  \new Staff <<
    \omit Staff.GridLine
    \new Voice {
      <f' a' c''>8
      \once \omit NoteHead q
      <g' b' d''>
      \once \omit NoteHead q
    }
    \new Voice { s8\p s\p s\p s\p }
  >>
  \layout {
    line-width = 9\cm
  }
}

\markup { Turn should be centered between noteheads: }
\score {
  \new Staff <<
    \new Voice { e''2 e''2 }
    \new Voice { s4 s\turn }
  >>
  \layout {
    line-width = 6.5\cm
  }
}


lyrExample = <<
  \new Staff <<
    \new Voice { s1*3 } % needed for gridLines
    \new Voice { d'2 d' <f' g'>1 s1 }
  >>
  \new Lyrics { \lyricmode { foo2 bar mmmm1 foo2 bar } }
>>

\markup { Lyrics default-aligned (centered): }
\score {
  \lyrExample
}

\markup { Lyrics right-aligned: }
\score {
  \lyrExample
  \layout {
    \context {
      \Score
      \override LyricText.self-alignment-X = #RIGHT
    }
  }
}

\markup { X-alignment-extent = 0, Lyrics right-aligned: }
\score {
  \lyrExample
  \layout {
    \context {
      \Score
      \override LyricText.self-alignment-X = #RIGHT
      \override PaperColumn.X-alignment-extent = #'(0 . 0)
    }
  }
}
