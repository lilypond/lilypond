
blah =
staff {melodic
	music{ 	$
\duration { 8 }
			\octave{ }
			\plet { 6/9 } |[ a a a a a a a a a ] \plet { 1/1 }
			\octave{ ' }
			\plet { 6/9 } |[ a a a a a a a a a ] \plet { 1/1 }
			\octave { ` } 
			\plet { 6/9 } [ `c `g d a 'e 'b ''f '''c '''g ] \plet { 1/1 }
			\plet { 6/9 } [ '''g '''c ''f 'b 'e a d `g `c ] \plet { 1/1 }
			\octave{ ' }
			\plet { 6/9 } [ `c `g d a 'e 'b ''f '''c '''g ] \plet { 1/1 }
			\plet { 6/9 } [ '''g '''c ''f 'b 'e a d `g `c ] \plet { 1/1 }
			\octave { } 
			\plet { 2/3 } [ c g 'd ] \plet { 1/1 }
			\plet { 2/3 } [ 'd g c ] \plet { 1/1 } 
			\plet { 2/3 } [ f 'c 'g ] \plet { 1/1 }
			\plet { 2/3 } [ 'g 'c f ] \plet { 1/1 } 
			\octave{ ' }
			\plet { 2/3 } [ c g 'd ] \plet { 1/1 }
			\plet { 2/3 } [ 'd g c ] \plet { 1/1 } 
			\plet { 2/3 } [ f 'c 'g ] \plet { 1/1 }
			\plet { 2/3 } [ 'g 'c f ] \plet { 1/1 }

c1
	\duration { 8}
	[c-> d-> e-> f->][g-> a-> b-> 'c->] % 1
\octave{'}	['c-^ b-^ a-^ g-^][f-^ e-^ d-^ c-^]
\octave{}	['c-^ b-^ a-^ g-^][f-^ e-^ d-^ c-^]
	[c-. d-. e-. f-.][g-. a-. b-. 'c-.]
	['c-- b-- a-- g][f-- e-- d c--] % 5
	[c-\portato d-\portato e-\portato f-\portato]
		[g-\portato a-\portato b-\portato 'c-\portato]
	['c-\upbow b-\upbow a-\downbow g-\downbow]
		[f-\downbow e-\downbow d-\upbow c-\upbow]
	[c-| d-| e-| f-|][g-| a-| b-| 'c-|]
	['c b a g][f e d c]
	[c d e f][g a b 'c] % 10 
	|['c b a g][f e d c]
			
		$}
}

score {
	staff {
	blah
	}
	paper {
		symboltables { table_sixteen}
		unitspace 1.5 cm
		geometric 1.4
	}
	commands  {
                meter {6*8}
                skip 36*8
		meter {4*4}
	}
}
