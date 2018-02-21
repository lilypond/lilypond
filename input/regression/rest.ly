\version "2.21.0"

\header{
  texidoc="
There is a big variety of rests.  Note that the dot of 8th, 16th and 32nd 
rests rest should be next to the top of the rest.  All rests except the 
whole rest are centered on the middle staff line.
"
}

\paper { ragged-right = ##t } 

{
  \time 4/4
  r \longa * 1/4  r\breve * 1/2 
  r1 r2 r4 r8 r16 r32 r64 r128 r256 r512 r1024 r
  \time 6/4
  r1. r2. r4. r8. r16. r32. r64. r128. r256. r512. r1024. r
}

