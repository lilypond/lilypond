\version "2.19.21"
\header {
  texidoc = "With the @code{\\lyricsto} mechanism, individual lyric
    lines can be associated with one melody line. Each lyric line
can be tuned to either follow or ignore melismata."
}


<<
  \new Voice = "bla" \relative {
    \autoBeamOff
    c''2( d4) e8[ c b c] f4
  }
  \new Lyrics  \lyricsto "bla"  { bla ab blob blob }
  \new Lyrics  \lyricsto "bla"  {
    bla 

    \set ignoreMelismata = ##t
    
    blob

    %% note: effect of ignoreMelismata delayed one time step.
    \unset ignoreMelismata 
    blob
    
    blob
  }
  
  \new Lyrics  \lyricsto "bla"  {
    nes ted lyrics voice with more words than no tes
  }
>>
  
