\version "2.16.0"

\header {
  texidoc="
A second book-level header block and headers nested in bookpart and score should not clear values from the first header block.  This score should show composer, piece, subtitle and title."
}

\book {
  \header {
    title = "Title incorrect (to be superseded at book level)"
    subtitle = "Subtitle incorrect (to be superseded in bookpart)"
    composer = "Composer correct (set in book)"
    piece = "Piece incorrect (to be superseded in score)"
  }
  % This should replace title without affecting other fields
  \header {
    title = "Title correct (superseded at book level)"
  }
  \bookpart {
    % This should replace subtitle without affecting other fields
    \header {
      subtitle = "Subtitle correct (superseded in bookpart)"
    }
    \markup \vspace #2
    \markup { \bold Note: title, subtitle, piece, and composer expected. }
    \markup \vspace #2
    \score {
      \new Staff { c'1 }
      \header {
        % This should replace piece without affecting other fields
        piece = "Piece correct (superseded in score)"
      }
    }
  }
}
