\version "2.19.25"

\header {
  texidoc="If a score has a @code{\header} block which defines a title,
  this title should override any title defined in a @code{\header} block
  of the score's enclosing @code{\bookpart} or @code{\book} (or a title
  defined in a top-level @code{\header} block) when naming the MIDI
  sequence generated from the score.  Otherwise, if the score has no title
  defined, the MIDI sequence generated from the score should get named
  using the title defined in the @code{\header} block of the nearest
  enclosing @code{\bookpart}, @code{\book}, or top-level scope that
  contains a title definition."
  title = "Top-level title"
}

music = \new Staff { c1 }

% Book with a title defined in a \header block, and book parts
\book {
  \header { title = "Book" }

  % score without a \header block outside of any book part -- the MIDI
  % sequence should get the title of the book as its name
  \score {
    \music
    \layout { }
    \midi { }
  }

  % score with a \header and a title outside of any book part -- the MIDI
  % sequence should be named with the title from this \header block
  \score {
    \music
    \header { title = "Score in a \book" }
    \layout { }
    \midi { }
  }

  % Book part with a \header block and a title
  \bookpart {
    \header { title = "Book part" }

    % score without a \header block -- the MIDI sequence should get its name
    % from the title of the enclosing book part
    \score {
      \music
      \layout { }
      \midi { }
    }

    % score with a \header (and a title) of its own -- the MIDI sequence
    % should get its name from the title in this \header block
    \score {
      \music
      \header { title = "Score in a book part (w/ a title) of a book" }
      \layout { }
      \midi { }
    }
  }

  % Book part without a \header block
  \bookpart {

    % score without a \header block -- the MIDI sequence should be named
    % using the title from the enclosing book
    \score {
      \music
      \layout { }
      \midi { }
    }

    % score with a \header block and title -- the MIDI sequence should get
    % its name from the title in this \header block
    \score {
      \music
      \header { title = "Score in a book part (w/o a title) of a book" }
      \layout { }
      \midi { }
    }
  }

}
