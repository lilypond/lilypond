\version "1.3.146"
\header{
filename = 	"chord-table.ly"
copyright =  	"public domain"
enteredby =  	"jcn"
}

tab =  \notes\transpose c'''\chords{
	c1 c:m c:4 c:m4 c:5+ c:5- c:dim c:5-.5+ c:6\break %c:m6\break
}

\score{
	\context StaffGroup <
		\context Staff=c \notes\transpose c\tab
		\context ChordNames=c \notes\transpose c\tab
		\context Staff=cis \notes\transpose cis\tab
		\context ChordNames=cis \notes\transpose cis\tab
		\context Staff=des \notes\transpose des\tab
		\context ChordNames=des \notes\transpose des\tab
		\context Staff=d \notes\transpose d\tab
		\context ChordNames=d \notes\transpose d\tab
		\context Staff=es \notes\transpose es\tab
		\context ChordNames=es \notes\transpose es\tab
		\context Staff=e \notes\transpose e\tab
		\context ChordNames=e \notes\transpose e\tab
		\context Staff=fis \notes\transpose fis\tab
		\context ChordNames=fis \notes\transpose fis\tab
		\context Staff=g \notes\transpose g\tab
		\context ChordNames=g \notes\transpose g\tab
		\context Staff=as \notes\transpose as\tab
		\context ChordNames=as \notes\transpose as\tab
		\context Staff=a \notes\transpose a\tab
		\context ChordNames=a \notes\transpose a\tab
		\context Staff=bes \notes\transpose bes,\tab
		\context ChordNames=bes \notes\transpose bes,\tab
		\context Staff=b \notes\transpose b,\tab
		\context ChordNames=b \notes\transpose b,\tab
	>
	\paper{
		textheight = \vsize - 4.0 * \staffheight
	}

}


