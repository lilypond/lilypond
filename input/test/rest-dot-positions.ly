\include "paper16.ly"
\paper  {
}

muz = \notes \relative c'' {
    \time 6/1
    r\longa r\breve |
    r\longa. |
    r\breve. r |
    \time 3/1
    r\breve r1 |
    r\breve. |
    r1. r |
    \time 3/2
    r1 r2 |
    r1. |
    r2. r |
    \break
    \time 3/4
    r2 r4 |
    r2. |
    r4. r |
    \time 3/8
    r4 r8 |
    r4. |
    r8. r |
    \time 3/16
    r8 r16 |
    r8. |
    r16. r |
    \break
    \time 3/32
    r16 r32 |
    r16. |
    r32. r |
    \time 3/64
    r32 r64 |
    r32. |
    r64. r |
    \time 3/128
    r64 r128 |
    r64. |
    r128. r |
  }


\score {
  \context Staff  {
      \muz \break
      <
	  \context Voice=one { \voiceOne \muz }
	  \context Voice=two { \voiceTwo \muz }
      >
  }
}
