\version "1.3.146"

\header{
texidoc="
Rests.  Note that the dot of 8th, 16th and 32nd rests rest should be
next to the top of the rest.  All rests except the whole rest are
centered on the middle staff line.
"
}


	\score { \notes {
	\time 4/4
r \longa * 1/4  r\breve * 1/2 
r1 r2 r4 r8 r16 r32 r64 r128 r128
\time 6/4
r1. r2. r4. r8. r16. r32. r64. r128. r128.
}}
