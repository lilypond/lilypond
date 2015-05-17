
\header { texidoc = "In this example, solo1
	  should not be printed over the 1st note, because of the slur
	  which is present from the one-voice to the two-voice situation."

}

\version "2.19.21"
\paper { ragged-right = ##t } 

\new Staff
    \partcombine  \relative {
	bes'2(
	 a4)
	}
     \relative {
	r2 cis'4
    }
