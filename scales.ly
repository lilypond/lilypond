
blah =
staff {melodic
	music{ 	$ c1
	\duration { 8}
	[c-> d-> e-> f->][g-> a-> b-> 'c->]
\octave{'}	['c-^ b-^ a-^ g-^][f-^ e-^ d-^ c-^]

\octave{}	['c-^ b-^ a-^ g-^][f-^ e-^ d-^ c-^]

	[c-. d-. e-. f-.][g-. a-. b-. 'c-.]
	['c-- b-- a-- g][f-- e-- d c--]
	[c-\portato d-\portato e-\portato f-\portato][g-\portato a-\portato b-\portato 'c-\portato]
	['c-\upbow b-\upbow a-\downbow g-\downbow][f-\downbow e-\downbow d-\upbow c-\upbow]
	[c-| d-| e-| f-|][g-| a-| b-| 'c-|]
	['c b a g][f e d c]
	[c d e f][g a b 'c]
	['c b a g][f e d c]
		$}
	commands {	
	}
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
	commands  {meter 4 4

		skip 18:0
	}
}