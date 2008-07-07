\version "2.11.51"
\header {
  texidoc = "With the @code{\\lyricsto} mechanism, individual lyric
    lines can be associated with one melody line. Each lyric line
can be tuned to either follow or ignore melismata."
}


<<
  \new Voice = "bla" \relative c'' {
    \autoBeamOff
    c2( d4) e8[ c b c] f4
  }
  \lyricsto "bla"  \new Lyrics  { bla ab blob blob }
  \lyricsto "bla"  \new Lyrics  {
    bla 

    \set ignoreMelismata = ##t
    
    blob

    %% note: effect of ignoreMelismata delayed one time step.
    \unset ignoreMelismata 
    blob
    
    blob
  }
  
  \lyricsto "bla"  \new Lyrics  {
    nes ted lyrics voice with more words than no tes
  }
>>
  
