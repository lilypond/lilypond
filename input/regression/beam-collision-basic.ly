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
    \repeat unfold 8 { c'8[ c] }
  }
    \new Voice \relative {
      \voiceThree
      \autoBeamOff
      f'' r e r
      d r c r
      b r a r
      g r f r
    } 
  >>
  \break

  %% The same with double collisions, to check for scaling problems.
  <<
    \new Voice {
    \voiceOne
    \repeat unfold 8 { c8[ c] }
  }
    \new Voice \relative {
      \voiceThree
      \autoBeamOff
      f'' f e e
      d d c c
      b b a a
      g g f f
    } 
  >>
  \break
  
  <<
     \new Voice {
       \repeat unfold 8 \relative {
	 \voiceOne
	 c'8[
	 \voiceTwo
	 c'']
       }
     }
     \new Voice \relative {
       \voiceFour
       s8 f' 
       s8 g
       s8 a
       s8 b
       s8 c
       s8 d
       s8 e
     }
   >>

  \break
   <<
     \new Voice {
       \repeat unfold 8 \relative {
	 \voiceOne

	 %% We must use a wider interval, otherwise the beam will be
	 %% positioned to descend.
	 a8[
	 \voiceTwo
	 c'']
       }
     }
     \new Voice \relative {
       \voiceFour
       \autoBeamOff
       \stemUp f'' \stemDown f,
       \stemUp e' \stemDown g,
       \stemUp d' \stemDown a
       \stemUp c \stemDown b
     }
   >>
}

