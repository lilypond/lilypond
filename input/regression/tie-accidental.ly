\header { texidoc = "
  lilypond should flip the tie's direction
  to avoid a collision with the sharp.
" }


\version "2.11.51"

\paper {
  debug-tie-scoring = ##t
  ragged-right = ##t
}

\relative c'' {
  \override Tie #'layer = #2
  a ~ <fis a>
}

% EOF
