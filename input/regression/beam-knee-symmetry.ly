
\version "2.3.22"
\header {

    texidoc = "Point-symmetric beams should receive the same
    quanting. There is no up/down bias in the quanting code."

}

\score{
    \relative c'{
	 a8[ b'' a,, b'']
	 b8[ a,, b'' a,,]
    }
    \layout{
	raggedright = ##t 
    }
}
