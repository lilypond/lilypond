\header{
  texidoc = "Concave beams should be horizontal.  However, what exactly
it is that makes a beam concave is still unclear.

Beams 1 and 3 should be sloped, 2 and 4 should be horizontal.  Two
sane attempts of calculating concaveness of a beam fail to distinguish
beams this way."

}

\score{
  \notes\relative c'{
    \property Voice.Beam \set #'debug-concave = ##t

%%%  \property Voice.Beam \set #'concaveness = #0.8
%%%  \property Voice.Beam \set #'concaveness-no-slope = ##f
%%%  \property Voice.Beam \set #'concaveness-square = ##f

    
    
    \property Voice.Beam \set #'concaveness-no-slope = ##t
    %%\property Voice.Beam \set #'concaveness = #0.25
    
    %% this gives what baerenreiter does, but it's too kludgy
    %% to make much sense
    \property Voice.Beam \set #'concaveness-square = ##t
    \property Voice.Beam \set #'concaveness = #0.08
    
%% This case seems easy: second beam should be horizontal.
    
    %% SCS-I Menuet I, m15
    %% sloped
    %% slope = -0.5ss
    %% concaveness: 0.50
    %% concaveness-no-slope: 0.25
    %% concaveness-no-slope^2: 0.06
    \clef bass
    \time 3/4
    \key g\major
    a8 g fis e b dis
    
    %% SCS-I Menuet II, m20
    %% horizontal
    %% slope = 0
    %% concaveness: 1.12
    %% concaveness-no-slope: 0.38
    %% concaveness-no-slope^2: 0.09
    \key f\major
    fis,^"horiz." a c es d c

%%% Sarabande: the first beam, obviously more concave, is not horizontal,
%%% but is matched with the next beam in the piece: context.
    
    %% Sarabande: m24
    %% sloped
    %% concaveness: 0.75
    %% concaveness-no-slope: 0.00
    %% concaveness-no-slope^2: 0.00
    \stemUp
    [d,16 a' b cis]

    %% Sarabande: m25
    %% horizontal
    %% concaveness: 0.50
    %% concaveness-no-slope: 0.25
    %% concaveness-no-slope^2: 0.12
    [a'16^"horiz." b c b]
    
% Hmm.  Concaveness of both: 1.75
%     %% SCS-VI Prelude, m81
%     %% slope = 0.0
%     \stemBoth
%     \key d\major
%     [e,8 cis a']
    
%     %% SCS-VI Prelude, m82
%     %% slope = 0.1ss (possibly b.o. context?)
%     [g, e' cis]
    
  }
  \paper{
    linewidth = -1.0
  }
}

%% Local variables:
%% LilyPond-indent-level:2
%% End:
