
\version "2.17.6"
\header{

  texidoc="Simple beams on middle staffline are allowed to be
    slightly sloped, even if the notes have ledgers.  Beams reaching
    beyond middle line can have bigger slope."

}

\layout{
  ragged-right = ##t
  indent = 0
}

\relative c'{
  %%\override Staff.Stem.beamed-lengths = #'(3.50)
  %%  c8[ d]
  %%  d[ c]
  %% r2
  %% \override Staff.Stem.beamed-lengths = #'(3.26)

  a8[^"small slope" b]
  b[ a]

  c''[ b]
  b[ c]

				% baerenreiter test
  b,,[ c]
  c[ b]

  b''[ a]
  a[ b]

  c,,[ d]
  d[ c]
  
  a''[ g]
  g[ a]

  c,,[^"bigger slope" e]
  e[ c]
  
  a''[ f]
  f[ a]
}

