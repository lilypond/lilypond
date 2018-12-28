\version "2.21.0"

\header {
  texidoc = "Rests must begin and end simultaneously to be merged into the shared voice."
}

% rests of different durations beginning simultaneously, followed by
% unisilence

\score {
  \partCombine
    \relative f' { r4    r2 r8 r8 | r1 }
    \relative f' { r8 r8 r2 r4    | r1 }
}

% rests of different durations beginning simultaneously, followed by
% solo then a2.

\score {
  \partCombine
    \relative { r4   f'2. | r8 f e2. }
    \relative { r8 d' f2. | r4   e2. }
}

% mmrest and rest of different durations beginning simultaneously

\score {
  \partCombine
    \relative { r4 f'2. | R1 }
    \relative { R1     | r4 d'2. }
}
