


\header { texidoc = "Collision resolution tries to put notes with dots
  on the right side."
}

\version "2.4.0"
\layout { raggedright= ##t }

% todo: b2 (up) + b8 down looks strange compared to c2up + b8. (down)
\score{
        \context Staff  <<
                \clef "bass"
		{ b8. c'16 d'4 b8  c'8  d'4 b2 b2  }
		\\
		{ b2 b2 b8. a16 g4 b8 a g4 }
        >>
}



