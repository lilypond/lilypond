
\version "1.9.8"
\header{
texidoc="
Rests should not collide with beams, stems and noteheads.  Rests may
be under beams.  Rests should be move by integral number of spaces
inside the staff, and by half spaces outside.  Notice that the half
and whole rests just outside the staff get ledger lines in different
cases.
"
}

scale =  \notes \relative c' {
  c8 d e f g a b c c d e f g a b c

}
rests =  \notes             {
  r r r  r r r r r r r r r r r r r
} 
different = <<  {
      \notes \relative c'' {
        r8 a e4 a e
      }
    }\\ {
      
      \notes \relative c'' {
        r1
      }} >>

scales =  \context Staff \notes <<
	 {  r1 r2 r2   \scale    c''1 c'2 a'2 \rests  }
	 \\
	 { a'1 a'2 d'2 \rests r1 r2 r2  \scale }
>>

restsII =  \context Staff \notes {
	r4 r8
	\context Staff << {  r8 } \\{  r8} >>
	\context Staff << { r8} r8 \\{  r8} >>
	\context Staff << { r8} r8 r8 \\ {  r8} >>
	\context Staff << { r} \\ {  r} >>
	\context Staff << { r} r\\ { \stemDown r} >>
	\stemUp
	\transpose c c' {  c''8[ r8 c''8 c''8]
	 c8[ r8 c8 c8]
	 c8[ r8 r8 c'''8]	
	\stemDown
	 c8[ r8 c8 c8]
	 c''8[ r8 c''8 c''8]
	 c'8[ r8 r8 c'''8]
	\different	
	}
}

\score{
	\notes { 
		\scales 
		\restsII 
	}
}	



