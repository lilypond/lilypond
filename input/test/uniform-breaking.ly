%{
Hmm, ik vraag me af of dit al helemaal koel is.

  return abs (this_one.force_f_) + abs (prev.force_f_ - this_one.force_f_)
      + break_penalties;

Neem als voorbeeld iets dat lijkt op allemande: keuze tussen 2 of drie
maten per regel.

* 2 lange maten -> lelie kiest 2 /regel  :beetje los
* 3 korte -> lelie kiest 3 /regel        :beetje krap
* 2 korte, 1 lange -> 3/regel            :krap
* 1 korte, 2 lange -> 3/regel            :erg krap
* 3 lange -> 3/regel                     :urg krap

als je naar beloningen kijkt, kan ik me goed voorstellen dat sprong
van 'al wat krapper' naar los te groot wordt, en ze dus steeds krapper
wordt, tot urg krap aan toe, want kracht lineair?  Dat lijkt ook geval
in allemande.

Zie hoe eerst 10 en 9 mooi op 2maat/regel staan terwijl later tot 14
toe 3/regel.

Heb niet zomaar beter idee, nog.
%}

\score{
	\notes\relative c'{
		% 10
		c8 c c c c8 c c c 
		c8 c c c c8 c c c 
		c8 c c c c8 ces c ces

		% 9
		c8 c c c c8 c c c 
		c8 c c c c8 c c c 
		c8 c c c c8 c ces c

		% 1
		c4 c c c
		c4 c c c
		c4 c c c

		% 2
		c4 c c c
		c4 c c c
		c4 c c8 c c c

		% 3
		c4 c c c
		c4 c c c
		c8 c c c c8 c c c 

		% 4
		c4 c c c
		c4 c c8 c c c
		c8 c c c c8 c c c 

		% 5
		c4 c c c
		c8 c c c c8 c c c 
		c8 c c c c8 c c c 

		% 6
		c4 c c8 c c c
		% c4 c c c8 c
		c8 c c c c8 c c c 
		c8 c c c c8 c c c 

		% 7
		c8 c c c c8 c c c 
		c8 c c c c8 c c c 
		c8 c c c c8 c c c 

		% 8
		c8 c c c c8 c c c 
		c8 c c c c8 c c c 
		c8 c c c c8 c c ces

		% 9
		c8 c c c c8 c c c 
		c8 c c c c8 c c c 
		c8 c c c c8 c ces c

		% 10
		c8 c c c c8 c c c 
		c8 c c c c8 c c c 
		c8 c c c c8 ces c ces

		% 11
		c8 c c c c8 c c c 
		c8 c c c c8 c c c 
		c8 c c c ces8 c ces c

		% 12
		c8 c c c c8 c c c 
		c8 c c c c8 c c c 
		c8 c c ces c8 ces c ces

		% 13
		c8 c c c c8 c c c 
		c8 c c c c8 c c c 
		c8 c ces c ces8 c ces c

	}
	\paper {
		indent=0.0\mm;
		linewidth=90.0\mm;
	}
}


