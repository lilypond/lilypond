\version "2.10.0"
\header{
texidoc="

Markings can be attached to (invisible) barlines.
"
}

\layout { ragged-right = ##t }

onestaff = \new Staff\relative c''  {
	\set Staff.shortInstrumentName = instr
	\set Staff.instrumentName = instrument \mark "B"
	 c1 \mark "A" \break c2  c2 \break
}

grstaff =  \relative c''
\context GrandStaff <<
	\new Staff {

	\set Staff.shortInstrumentName = instr
	
	 \mark "B" \break c1 \mark "A" c2  }
	\new Staff { c1 c2  }
>>


\score {\grstaff}

