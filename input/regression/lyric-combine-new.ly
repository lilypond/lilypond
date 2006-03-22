\version "2.8.0"
\header {

  texidoc = "With the @code{\\lyricsto} mechanism, individual lyric
    lines can be associated with one melody line. For each lyric line,
    can be tuned whether to follow melismata or not."
  
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
  
