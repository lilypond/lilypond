\version "2.19.38"

\header {
  texidoc = "
@code{\\parallelMusic} does not complain about incomplete bars at its
end.
"
}

\parallelMusic vI,vII
\fixed c'
{
  f2 |
  a2 |
  d2 r2 |
  d2 r8 cis cis cis |
  d4 r4 \bar "|." |
  d4 r4 \bar "|." |
}

global =
{
  \key d\minor
  \time 4/4
  \partial 2
}

\score {
  \new StaffGroup
  <<
    \new Staff { \global \vI }
    \new Staff { \global \vII }
  >>
}
