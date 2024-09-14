\version "2.25.21"

\header {
  texidoc = "@code{\\partial} works with polymetric staves."
}

\layout {
  \enablePolymeter
}

%% Weird beaming should make this test more sensitive to regressions in
%% calculating measurePosition.
\fixed c' <<
  \new Staff {
    \time 1,1,1,2 5/8
    \partial 8*2
    \repeat unfold 2 c8 |
    \repeat unfold 5 c8 |
  }
  \new Staff {
    \time 1,3 4/8
    \partial 8*3
    \repeat unfold 3 c8 |
    \repeat unfold 4 c8 |
  }
>>
