%% under construction

define(preludepattern, `[' $1 $2 $3 $2 `]')
define(preludehalfmeasure, preludepattern($1, $2, $3) 
	preludepattern($4, $2, $3))

define(intromeasure,
	preludehalfmeasure($1, $2, $3, $4)
	preludehalfmeasure($1, $2, $3, $4)|	
)
define(handymeasure, `intromeasure(translit($*,` ', `,'))')



trebleIntro=\notes \transpose c'' {
handymeasure(c' es d c)
handymeasure(as f e c)
handymeasure(b f es d)
handymeasure(c g f es)
handymeasure(es' as g es)
handymeasure(d' fis e d)
handymeasure(d' g fis d)
handymeasure(c' e d c)
handymeasure(c' f e c)
handymeasure(bes f es d)
handymeasure(bes g f es)
handymeasure(as g f es)
handymeasure(as d c Bes)
handymeasure(g Bes As es)
handymeasure(f c Bes A)
handymeasure(f d c B)
handymeasure(es c B G)
handymeasure(F es d f)
handymeasure(Fis c b es)
handymeasure(es c B G)
handymeasure(fis c B A)
handymeasure(g c B d)
handymeasure(as c B d)
}

bassIntro = \notes {
handymeasure(c g f es)
handymeasure(c as g f)
handymeasure(c as g f)
handymeasure(c es d g)
handymeasure(c c' bes as)
handymeasure(c a g fis)
handymeasure(Bes bes a g)
handymeasure(Bes g f e)
handymeasure(As as g f)
handymeasure(As d c f)
handymeasure(G es d g)
handymeasure(c es d As)
handymeasure(d f es as)
handymeasure(es g f as)
handymeasure(es a g f)
handymeasure(d f es as)
handymeasure(c f e as)
preludehalfmeasure(c, es, d, f)
preludehalfmeasure(bes, es, d, f)|
handymeasure(As c b d)
handymeasure(A es d c)
handymeasure(G es d f)
handymeasure(G es d c)
handymeasure(G es d f)
handymeasure(G es d f)
}


middlepiece = \notes
{
	\stemdown
	[G B d] 
	\stemup	f [as f ef] [b f d' b'] [as f e f] |
	\stemdown [G c es]]
	\stemup g [c' g fis g ] [es' c' g' es'] [c' as g as]| 
	\stemdown [G A fis] 
	\translator Staff=treble
	\stemsboth \transpose c'' { c [es c B c] [fis  c a fis] [es c B c] }
}




