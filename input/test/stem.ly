\version "1.7.18"
% regression, _so_ regression.  :)  =-gp

\header{ texidoc = "
This file tests the length of stems and placement 
of beams"
}

beamintervals = \notes{
  \time 7/4
  \stemUp
  \transpose c c'{
    [ c8 d ] [ c e ] [ c f ] [ c g ] [ c a ] [ c b ] [ c c' ] |
    [ c b, ] [ c a, ] [ c g, ] [ c f, ] [ c e, ] [ c d, ] [ c c, ] |
  }
  \transpose c c''{
    [ c b, ] [ c a, ] [ c g, ] [ c f, ] [ c e, ] [ c d, ] [ c c, ] |
    \stemDown
  }
  \transpose c c'''{
    [ c b, ] [ c a, ] [ c g, ] [ c f, ] [ c e, ] [ c d, ] [ c c, ] |
  }
  \transpose c c''{
    [ c b, ] [ c a, ] [ c g, ] [ c f, ] [ c e, ] [ c d, ] [ c c, ] |
    [ c d ] [ c e ] [ c f ] [ c g ] [ c a ] [ c b ] [ c c' ] |
  }
}

\score{
  \notes{
    \time 19/4

    %% 3.50 (standard) - 0.24 (beam-thickness / 2) = 3.26 ...
    %% yields beams almost as [Ross] wants them.
    
    %% Differences are only half a beam-thickness, probably giving
    %% Lily's beams a slightly better slope
    
    \property Staff.Stem \set #'beamed-lengths = #'(3.26)

    \relative c'{
      \stemUp
      g4 a b c d e f g a b c d e f g a b c d
      \stemDown
      d c b a g f e d c b a g f e d c b a g
    }      
    \beamintervals
    \transpose c d \beamintervals
    \transpose c e \beamintervals
    \transpose c f \beamintervals
    \transpose c g \beamintervals
    \transpose c a \beamintervals
    \transpose c b \beamintervals
  }
  \paper{
    indent = 0.0\mm
    }
}

%%% Local variables:
%%% LilyPond-indent-level:2
%%% End:

