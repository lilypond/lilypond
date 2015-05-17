\version "2.19.21"
\header {

  texidoc = "Kneed beams (often happens with cross-staff beams)
should look good when there are multiple beams: all the beams should
go on continuously at the staff change. Stems in both staves reach up
to the last beam.
"
}

\layout {
  ragged-right= ##t
}

\context PianoStaff \relative c' <<
  \new Staff = SA {
    \voiceTwo
    c8[ c16 \change Staff = SB \voiceOne c16 ]
    \oneVoice
    f[ g \change Staff = SA a c] 
  }
  \new Staff = SB \relative {
    \clef bass
    b8[ b16 \voiceOne b,,16 ]

    g'4\rest
    b,8[ b16 \voiceTwo b''16 b ]
  }
  
>>


