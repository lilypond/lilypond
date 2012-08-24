
\version "2.16.0"
\header {

    texidoc = "Point-symmetric beams should receive the same
quanting.  There is no up/@/down bias in the quanting code."

}
\layout{
  ragged-right = ##t 
}

\relative c'{
  a8[ b'' a,, b'']
  b8[ a,, b'' a,,]
}
