\version "2.11.51"
\header{
texidoc="

Markings can be attached to (invisible) barlines.
"
}

\layout { ragged-right = ##t }

\relative c''
\context GrandStaff <<
	\new Staff {

	\set Staff.shortInstrumentName = instr
	
	 \mark "B" \break c1 \mark "A" c2  }
	\new Staff { c1 c2  }
>>

