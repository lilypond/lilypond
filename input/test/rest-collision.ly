
scale = \notes \relative c' {
  c8 d e f g a b c c d e f g a b c

}
rests = \notes             {
  r r r  r r r r r r r r r r r r r
} 
different =< \context Voice = one {
      \stemup
      \notes \relative c'' {
        r8 a e4 a e
      }
    }
    \context Voice = two {
      \stemdown
      \notes \relative c'' {
        r1
      }} >

scales = \context Staff \notes <
	\context Voice=i { \stemup r1 r2 r2   \scale    c''1 c'2 a'2 \rests  }
	\context Voice = ii { \stemdown a'1 a'2 d'2 \rests r1 r2 r2  \scale }
>

restsII = \context Staff \notes {
	r4 r8
	\context Staff < { \stemup r8 } { \stemdown r8} >
	\context Staff < {\stemup r8} r8 { \stemdown r8} >
	\context Staff < {\stemup r8} r8 r8 { \stemdown r8} >
	\context Staff < {\stemup r} { \stemdown r} >
	\context Staff < {\stemup r} r { \stemdown r} >
	\stemup
	\transpose c'' { [c''8 r8 c''8 c''8]
	[c8 r8 c8 c8]
	[c8 r8 r8 c'''8]	
	\stemdown
	[c8 r8 c8 c8]
	[c''8 r8 c''8 c''8]
	[c'8 r8 r8 c'''8]
	\different	
	}
}

\score{
	\notes { 
		\scales 
		\restsII 
	}
}	

\version "1.3.59"; 
