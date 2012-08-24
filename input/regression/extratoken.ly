\header {
  texidoc = "A mode switching command like @code{\\lyricsto} will
`pop state' when seeing the lookahead token @code{\\time}, a music
function, after its non-delimited argument.  This must not cause the
extra token parsing state for the music function to disappear."
}

\paper {
  ragged-right = ##t
}

\version "2.16.0"

x=\lyrics { oh }

<< 
  \new Voice = m { c'4 r r }
  \lyricsto "m" \x
  \time 3/4
>>
