\header{
filename =	"chord-table.ly";
copyright = 	"public domain";
enteredby = 	"jcn";
}

tab = \notes\transpose c'''\chords{
	c1 c-m c-4 c-m4 c-5+ c-5- c-m5- c-5-.5+ c-6\break %c-m6\break
}

\score{
	\type StaffGroup <
		\type Staff=c \notes\transpose c\tab
		\type ChordNames=c \notes\transpose c\tab
		\type Staff=cis \notes\transpose cis\tab
		\type ChordNames=cis \notes\transpose cis\tab
		\type Staff=des \notes\transpose des\tab
		\type ChordNames=des \notes\transpose des\tab
		\type Staff=d \notes\transpose d\tab
		\type ChordNames=d \notes\transpose d\tab
		\type Staff=es \notes\transpose es\tab
		\type ChordNames=es \notes\transpose es\tab
		\type Staff=e \notes\transpose e\tab
		\type ChordNames=e \notes\transpose e\tab
		\type Staff=fis \notes\transpose fis\tab
		\type ChordNames=fis \notes\transpose fis\tab
		\type Staff=g \notes\transpose g\tab
		\type ChordNames=g \notes\transpose g\tab
		\type Staff=as \notes\transpose as\tab
		\type ChordNames=as \notes\transpose as\tab
		\type Staff=a \notes\transpose a\tab
		\type ChordNames=a \notes\transpose a\tab
		\type Staff=bes \notes\transpose bes,\tab
		\type ChordNames=bes \notes\transpose bes,\tab
		\type Staff=b \notes\transpose b,\tab
		\type ChordNames=b \notes\transpose b,\tab
	>
	\paper{
		textheight = \vsize - 4.0 * \staffheight;
	}

}
