\version "2.2.0"
\header{
texidoc="

Markings can be attached to (invisible) barlines.
"
}

\paper { raggedright = ##t }

onestaff = \new Staff\notes\relative c''  {
	\set Staff.instr = instr
	\set Staff.instrument = instrument \mark "B"
	 c1 \mark "A" \break c2  c2 \break
}

grstaff = \notes \relative c''
\context GrandStaff <<
	\new Staff {

	\set Staff.instr = instr
	
	 \mark "B" \break c1 \mark "A" c2  }
	\new Staff { c1 c2  }
>>


\score {\grstaff}

