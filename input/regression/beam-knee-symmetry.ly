#(ly:set-option 'old-relative)
\version "1.9.1"
\header {

    texidoc = "Point-symmetric beams should receive the same
    quanting. There is no up/down bias in the quanting code."

}

\score{
    \notes\relative c'{
	 a8[ b'' a,, b'']
	 b8[ a,, b'' a,,]
    }
    \paper{
	raggedright = ##t 
    }
}
