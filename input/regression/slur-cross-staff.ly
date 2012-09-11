
\version "2.16.0"
\header{
  texidoc="
Slurs behave decently when broken across a linebreak.
"
}



\context PianoStaff <<
  \new Staff = "one" \relative c'{
    \stemUp \slurUp
    c4( c \change Staff=two c  c) |
    \change Staff=one
    \stemUp \slurUp
    c4( c \change Staff=two c  c) |
    \stemUp \slurUp
    c4( c \change Staff=one c  c) |
    \change Staff=two
    \stemUp \slurUp
    c4( c \change Staff=one c  c) |
    \change Staff=two
    \stemUp \slurUp
    c4( \change Staff=one c c  c) |
    r2
    \change Staff=two
    \stemUp \slurUp
    c4( \change Staff=one c
    \break
    c  c)
    r2
    %%		\stemDown \slurDown
    %%		 c4( \change Staff=two c c \change Staff=one  c)
    \stemDown \slurDown
    d4( \change Staff=two c c \change Staff=one  d)
    \change Staff=two
    \stemUp \slurUp
    c4( \change Staff=one c c \change Staff=two  c)
    r1
  }
  \new Staff = "two" \relative c'{
    \clef bass
    s1 s1 s1 s1 s1 s1 s1 s1 s1 s1
  }
>>




