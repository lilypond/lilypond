\header {
  title = "Petite Ouverture à danser"
  subtitle = "4"
  source = ""
  composer = "Erik Satie (1866-1925)"
  enteredby = "jcn"
  copyright = "Public Domain"
}

\version "2.11.61"

global =  {
  \key a \minor
  \time 2/4
  s2*10
  \bar "||"
  s2*11
  \bar "||"
  \time 3/4
  s2.
  \bar "||"
  \time 2/4
  s2*4
  \bar "||"
  \time 3/4
  s2.*2
  \bar "||"
  \time 2/4
  s2*18
  \bar "|."
}
  
i = \context Staff \relative c''\new Voice {
  \voiceOne

  c8.( es16 bes4 ~ |  bes8) r c8.( bes16 | des4 c8. bes16 | c4 ~  c8) r |
  c4( f,8. as16 | bes4 ~  bes8) r | f8.( es16 f4 | es  f) |
  g8.( es16 f4 ~ |  f) f8 r

  % Au mouvement
  f4 ( g | a2 ~ |  a) | a4-- a-> ~ | a8 r b!8.( a16 |  b4) c-- ~ |
  c8 r b8.( d16 | a4 ~  a8) r | d4( cis | c! b8. d16 | a4 ~  a8) r

  a8.( g16 a4 ~  a8) r |

  a4-> ~ a8 r | g8.( a16  fis4) | e8.( d16 e4 | fis ~  fis8) r

  d4( d-- ~  d8) r e4( f!2 ~ |

  f4 ~  f8) r | es4( g | as bes ~ |  bes) c( |  b!2) | c4( d |  bes2) | c4~ c8 r |
  
  % copy from begin: 1-10
  c8.( es16 bes4 ~ |  bes8) r c8.( bes16 | des4 c8. bes16 | c4 ~  c8) r |
  c4( f,8. as16 | bes4 ~  bes8) r | f8.( es16 f4 | es  f) |
%  g8.( es16 f4 ~ |  f) f8 r ?
  g8.( es16 f4 ~ |  f) ~ f8 r
  
}

ii = \context Staff \relative c'\new Voice{
  \voiceTwo

  r8 <es as> r <des f> | r <es g> r <es as> | r <f as> r <f as> |
  r <es g> r <es g> | r <es as> r <as, des> | r <des f> r <des f> |
  r <as des> r <bes des> | r bes r <as c> | r <g des'> r <bes d> |
  r <a! c> r <a c>

  % Au movement
  r <as des> r <c e> | r <c f> r <d! f> | r <cis e> r <cis e> |
  r <c! f> r <d f> | r <d f> r <d g> | r <e g> r <e g> | r <e g> r <d g> |
  r <d fis> r <d fis> | r <fis b> r <fis a> | r <e a> r <d g> |
  r <d fis> r <d fis> |

  r <cis e> r <cis fis> r <cis f> |

  r <cis e> r <cis e> | r <b d> r <a d> | r <g b> r <a cis> |
  r <a cis> r <a cis> |

  \change Staff=bass\voiceOne
  r <g b> r <fis a> r <fis a> | r <g bes>
  \change Staff=treble\voiceTwo
  r <a c> r <a d> |

  r <bes d> r <bes d> | r <g c> r <bes d> | r <c es> r <d g> |
  r <d f> r <es g> | r <e! g> r <d fis> | r <e a> r <fis a> |
  r <es g> r <es g> | r <es g> r <es g> |

  
  % copy from begin: 1-10
  r8 <es as> r <des f> | r <es g> r <es as> | r <f as> r <f as> |
  r <es g> r <es g> | r <es as> r <as, des> | r <des f> r <des f> |
  r <as des> r <bes des> | r bes r <as c> | r <g des'> r <bes d> |
  r <a! c> r <a c>
 
}

lower = \context Staff  \relative c \new Voice{

  <as as'>4 <es es'> | r <as as'> | <des, des'> <f f'> | <c c'> r |
  <as as'> <des des'> | <bes bes'> r | <des des'> <bes bes'> |
  <g g'> <f f'> | <c' c'> <bes bes'> | <f f'> r |

  % Au movement
  <des' des'> <c c'> | <f f'> <d! d'!> | <a a'> r | <f' f'> <d d'> |
  r <f f'> | <e e'> <c c'> | r <g g'> | <d' d'> r | <b b'> <fis fis'> |
  <a a'> <b b'> | <d d'> r |

  <a a'> <fis fis'> r |

  <a a'> r | <b b'> <d d'> | <e e'> <a, a'> | <fis fis'> r |

  <g g'> <d d'> r | <g g'> <f! f'!> <d' d'> |

  <bes bes'> r | <c c'>  <bes bes'> | <as as'> <g g'> | <d' d'> <c c'> |
  <e! e'!> <b! b'!> | <a a'> <d d'> | <es es> r | <c c'> r
  
  % copy from begin: 1-10
  <as' as'>4 <es es'> | r <as as'> | <des, des'> <f f'> | <c c'> r |
  <as as'> <des des'> | <bes bes'> r | <des des'> <bes bes'> |
  <g g'> <f f'> | <c' c'> <bes bes'> | <f f'> r |

}

\score {
    \context PianoStaff <<
	\new Staff = "treble" << 
	    \global 
	    \clef violin
	    \i
	    \ii
	>>
	\new Staff = "bass" <<
	    \global
	    \clef bass
	    \lower
	>>
    >>
    \layout {
	\context {
	    \Score
	    \override SpacingSpanner #'spacing-increment = #3
	}
    }
    
  \midi {
    \context {
      \Score
      tempoWholesPerMinute = #(ly:make-moment 60 4)
      }
    }


}

%% Local Variables:
%% coding: utf-8
%% End:
