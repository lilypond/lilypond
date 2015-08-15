\version "2.19.25"

\header {
  texidoc="The MIDI sequence generated from a score should get its name
  from the title defined in the score's @code{\header} block (if any).
  The title used for layout can be overridden for MIDI output by
  specifying a separate @code{midititle} in the @code{\header} block.
  If the score does not define a title of its own, and has no enclosing
  @code{\bookpart}, @code{\book}, or top-level scope with a @code{\header}
  block that defines a title, either, the MIDI sequence should get the
  default name."
}

music = \new Staff { c1 }

% Book without a \header block
\book {

  % score with a \header block including a title -- the MIDI sequence
  % should get its name from this \header block
  \score {
    \music
    \header { title = "Title shared between layout and MIDI" }
    \layout { }
    \midi { }
  }

  % score with no title, but a midititle defined in a \header block --
  % the MIDI sequence should be named using the midititle in this \header
  % block
  \score {
    \music
    \header { midititle = "No title for layout, but a title for MIDI" }
    \layout { }
    \midi { }
  }

  % score with a title and a midititle defined in a \header block -- the
  % MIDI sequence should get the midititle in this \header block as its
  % name
  \score {
    \music
    \header {
      title = "Title for layout"
      midititle = "Title for MIDI"
    }
    \layout { }
    \midi { }
  }

  % Book part with no \header block
  \bookpart {

    % score without a \header -- the MIDI sequence should get the default
    % name
    \score {
      \music
      \layout { }
      \midi { }
    }
  }

}
