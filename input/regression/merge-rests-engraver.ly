\version "2.20.0"

\header {
  texidoc = "Test for merging rests in different voices."
}

\paper {
  ragged-right = ##f
}

voiceA = \relative {
  % no rest merges
  c''4 r c c |

  % does not combine differently written rests
  c4 r r2 |

  % all rests merged
  r2^"Up" r4 r8 r16 r32 r64 r128 r |

  % multi-measure rests are combined
  R1^"Upper text" |

  % compressed multi-measure rests are combined
  R1*3 |

  % combining between beams, slurs
  c8[( r c]) r c16[( r c] r c[ r c]) r |

  % combining in tuplets
  \tuplet 3/2 { c8 r r } r4 \tuplet 3/2 { c4 r r } |

  % accents on rest, dynamics still aligned
  r4->\f\> r-. r r\! |

  % Non-multimeasure whole rests merged at the correct vertical position
  \time 8/4
  r1 r1

  % Ensure when suspending merging rests are in their usual positions
  \time 4/4
  \set Staff.suspendRestMerging = ##t
  r4 r8
  \set Staff.suspendRestMerging = ##f
  r8 r2 |

  % Don't merge pitched rests
  c4\rest d\rest e\rest f\rest |
}

voiceB = \relative {
  r2 c'4 r |
  c4 r r r |
  r2_"Down" r4 r8 r16 r32 r64 r128 r |
  R1_"Lower text" |
  R1*3 |
  c8[( r c]) r c16[( r c] r c[ r c]) r |
  \tuplet 3/2 { c8 r r } r4 \tuplet 3/2 { c4 r r } |
  r4-> r-. r r |
  r1 r1 |
  r4 r8 r r2 |
  r4 r r r |
}

voiceC = \relative {
  s1*2 |
  r2 r4 r8 r16 r32 r64 r128 r | % Combines rests from more than 2 voices
}

\score {
  \new Staff \with {
      \consists "Merge_rests_engraver"
  } <<
    \compressFullBarRests
    \new Voice { \voiceOne \voiceA }
    \new Voice { \voiceTwo \voiceB }
    \new Voice { \voiceThree \voiceC }
  >>
}
