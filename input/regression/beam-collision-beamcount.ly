\version "2.19.21"
\header {
  texidoc = "Manual beams do not collide with notes."
}

\layout {
%  debug-beam-scoring = ##t
  indent = #0.0
}

\relative \new Staff {

  <<
    \new Voice {
      \voiceOne
      \repeat unfold 4 { c'8[ c] }
    }
    \new Voice \relative {
      \voiceThree
      \autoBeamOff
      b' r a r
      g r f r
    } 
  >>

   |
  
  <<
    \new Voice {
      \voiceOne
      \repeat unfold 4 { c16[ c] }
    }
    \new Voice \relative {
      \voiceThree
      \autoBeamOff
      b' r a r
      g r f r
    } 
  >>

}
