\version "1.5.68"

\header{
texidoc="
Ties are strictly horizontal.  They are placed in between note heads.
The horizontal middle should not overlap with a staffline.
"
}
\score{
	\notes\relative c''{
		%b2~b4~b8~b16~b32~b64 r64\break
		%a2~a4~a8~a16~a32~a64 r64
		\time 8/4
		d1 ~ d2~d4~d8~d16~d32~d64 r64\break
		a1~ a2~a4~a8~a16~a32~a64 r64
		%c2~c4~c8~c16~c32~c64 r64
	}
	\paper{
		linewidth=0.
	}
}
