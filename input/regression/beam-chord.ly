\version "1.9.1"
\header {
    
texidoc = "If the first and last outer notes of a beam are the same,
the beam should be be horizontal.  "

}
\score{
	\notes	\transpose c' c'{

		 a'8[ <<a' g''>>]
		 a'16[ <<a' g''>>]
		 a'32[ <<a' g''>>]
		 a'8[ <<f' c''>>]
		 a'16[ <<f' c''>>]
		 a'32[ <<f' c''>>]
		 a'16[ <<f' c''>>]
		 c8[ <<c e,>>]
		 c16[ <<c e,>>]
		 c32[ <<c e,>>]
	}
	\paper{
	    raggedright = ##t 
	}
}

