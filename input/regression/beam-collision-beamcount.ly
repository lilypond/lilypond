\version "2.16.0"
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
