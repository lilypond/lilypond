
\version "2.3.4"
% candidate for regression.  -gp
\header { texidoc = "@cindex Rest Dot Positions
Dots of rests should follow the rest positions. " }

muz =  \relative c'' {
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
      <<
	 {  \muz } \\
	 {  \muz }
      >>
  }
	\paper{}
}

