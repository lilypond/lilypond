\version "2.16.0"

\header  {
texidoc = "a staff should die if there is reference to it."
}
\layout {
  ragged-right = ##t
  line-width = 2.5 \cm
  indent = 0.0
}

{
  << \new Staff =  "q" \new Voice ="V" c1  >>
  %% no \break, BreakEvent causes spurious staff
  \new RhythmicStaff = "R" \new Voice = "RV" c 
}
