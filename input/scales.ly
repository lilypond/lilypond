
blah = \staff{  "melodic" 
	\melodic {
		\duration { 8 }		  \meter {6/8}	
 		
		\octave{ c }
			 |[ a cr a a a a a a a rc a ]6/9 
			\octave{ c }
			 |[ a decr a a a a a a a rc a ]6/9 
			\octave { c' } 
			 [ 'c 'g d a e' b' f'' c''' g''' ]6/9
			 [ g''' c''' f'' b' e' a d 'g 'c ]6/9
			\octave{ c'' }
			 [ 'c 'g d a e' b' f'' c''' g''' ]6/9
			 [ g''' c''' f'' b' e' a d 'g 'c ]6/9
			\octave { ''c}   
			 [ c g d' ]2/3 
			 [ d' g c ]2/3  
			 [ f c' g' ]2/3 
			 [ g' c' f ]2/3  
			\octave{ c }
			 [ c g d' ]2/3 
			 [ d' g c ]2/3  
			 [ f c' g' ]2/3 
			 [ g' c' f ]2/3 
 [ g' c' f ]2/3 
  \meter {4/4}
	
c1
	\duration { 8}
	[c-> d-> e-> f->][g-> a-> b-> c'->] % 1
\octave{c'}	[c'-^ b-^ a-^ g-^][f-^ e-^ d-^ c-^]
\octave{c}	[c'-^ b-^ a-^ g-^][f-^ e-^ d-^ c-^]
	[c-. d-. e-. f-.][g-. a-. b-. c'-.]
	[c'-- b-- a-- g][f-- e-- d c--] % 5
	[c-portato d-portato e-portato f-portato]
		[g-portato a-portato b-portato c'-portato]
	[c'-upbow b-upbow a-downbow g-downbow]
		[f-downbow e-downbow d-upbow c-upbow]
	[c-| d-| e-| f-|][g-| a-| b-| c'-|]
	[c' b a g][f e d c]
	[c d e f][g a b c'] % 10 
	|[c' b a g][f e d c]
			
	}
}

\score{
	blah
	\paper{
		\symboltables {table_sixteen}
		\unitspace 1.5 \cm
		\geometric 1.4
	}
}
