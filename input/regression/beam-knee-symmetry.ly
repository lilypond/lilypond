
\version "2.7.13"
\header {

    texidoc = "Point-symmetric beams should receive the same
    quanting. There is no up/down bias in the quanting code."

}
\layout{
  raggedright = ##t 
}

\relative c'{
  a8[ b'' a,, b'']
  b8[ a,, b'' a,,]
}
