\version "1.7.6"
\header{
filename = 	"chord-table.ly"
copyright =  	"public domain"
enteredby =  	"jcn"
}

tab =  \notes\transpose c c''\chords{
	c1 c:m c:4 c:m4 c:5+ \break
	c:5- c:dim c:5-.5+ c:6 c:m6\break
	c:4.6 c:7 c:m7 c:4.7 c:m4.7\break
	c:5+.7 c:5-.7 c:m5-.7 c:maj c:m.maj\break
	c:4.maj c:m4.maj c:5+.maj c:m4+.maj c:5-.maj\break
	c1:m5-.maj c:dim7 c:5-.5+.7 c:6.maj c:m6.maj\break
	c:4.6.maj c:9 c:m9 c:4.9 c:m.4.9\break
}

\score{
%	\context StaffGroup <
        <
		\context Staff=c \notes\transpose c' c\tab
		\context ChordNames=c \notes\transpose c' c\tab
%{		
		\context Staff=cis \notes\transpose c' cis\tab
		\context ChordNames=cis \notes\transpose c' cis\tab
		\context Staff=des \notes\transpose c' des\tab
		\context ChordNames=des \notes\transpose c' des\tab
		\context Staff=d \notes\transpose c' d\tab
		\context ChordNames=d \notes\transpose c' d\tab
		\context Staff=es \notes\transpose c' es\tab
		\context ChordNames=es \notes\transpose c' es\tab
		\context Staff=e \notes\transpose c' e\tab
		\context ChordNames=e \notes\transpose c' e\tab
		\context Staff=fis \notes\transpose c' fis\tab
		\context ChordNames=fis \notes\transpose c' fis\tab
		\context Staff=g \notes\transpose c' g\tab
		\context ChordNames=g \notes\transpose c' g\tab
		\context Staff=as \notes\transpose c' as\tab
		\context ChordNames=as \notes\transpose c' as\tab
		\context Staff=a \notes\transpose c' a\tab
		\context ChordNames=a \notes\transpose c' a\tab
		\context Staff=bes \notes\transpose c' bes,\tab
		\context ChordNames=bes \notes\transpose c' bes,\tab
		\context Staff=b \notes\transpose c' b,\tab
		\context ChordNames=b \notes\transpose c' b,\tab
%}		
	>
	\paper{
		textheight = \vsize - 4.0 * \staffheight
	}

}


%% new-chords-done %%
