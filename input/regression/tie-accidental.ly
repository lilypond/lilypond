\header { texidoc = "
  lilypond should flip the tie's direction
  to avoid a collision with the sharp.
" }


\version "2.19.21"

\paper {
  debug-tie-scoring = ##t
  ragged-right = ##t
}

\relative {
  \override Tie.layer = #2
  a' ~ <fis a>
}

% EOF
