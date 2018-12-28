
\header { texidoc = "In this example, solo1
	  should not be printed over the 1st note, because of the slur
	  which is present from the one-voice to the two-voice situation."

}

\version "2.21.0"
\paper { ragged-right = ##t } 

\new Staff
    \partCombine  \relative {
	bes'2(
	 a4)
	}
     \relative {
	r2 cis'4
    }
