
\header { texidoc = "Solo/Solo2 also is global: In this example, solo1
	  should not printed over the 1st note, because the voice
	  switch would kill the slur."

}

\version "2.1.18"

\score {
    \new Staff
    \partcombine \notes \relative c'' {
	bes2(
	 a4)
	}
    \notes \relative c' {
	r2 cis4
    }
}
