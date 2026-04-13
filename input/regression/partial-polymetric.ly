\version "2.25.35"

\header {
  texidoc = "@code{\\partial} works with per-staff timing."
}

\layout {
  \enablePerStaffTiming
}

%% Weird beaming should make this test more sensitive to regressions in
%% calculating measurePosition.
\fixed c' <<
  \new Staff {
    \time 1,1,1,2 5/8
    \partial 8*2
    \*2 c8 |
    \*5 c8 |
  }
  \new Staff {
    \time 1,3 4/8
    \partial 8*3
    \*3 c8 |
    \*4 c8 |
  }
>>
