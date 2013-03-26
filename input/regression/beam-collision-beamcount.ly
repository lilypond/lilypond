\version "2.17.15"
\header {
  texidoc = "Manual beams do not collide with notes."
}

\layout {
%  debug-beam-scoring = ##t
  indent = #0.0
}

\relative c' \new Staff {

  <<
    \new Voice {
      \voiceOne
      \repeat unfold 4 { c8[ c] }
    }
    \new Voice \relative c'' {
      \voiceThree
      \autoBeamOff
      b r a r
      g r f r
    } 
  >>

   |
  
  <<
    \new Voice {
      \voiceOne
      \repeat unfold 4 { c16[ c] }
    }
    \new Voice \relative c'' {
      \voiceThree
      \autoBeamOff
      b r a r
      g r f r
    } 
  >>

}
