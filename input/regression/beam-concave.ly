\version "1.5.68"
\header{

texidoc = "Concave beams should be horizontal. informally spoken,
  concave refers to the shape of the notes that are opposite a
  beam. If an up-beam has high notes on its center stems, then we call
  it concave.  This example shows borderline cases. Only the beams
  that are marked `horiz' should be printed horizontally.  " }


%{
 However, what exactly
it is that makes a beam concave is still unclear.

Beams 1 and 3 should be sloped, 2 and 4 should be horizontal.  Two
sane attempts of calculating concaveness of a beam fail to distinguish
beams this way."
%}


\score{
  \notes\relative c'{

%% This case seems easy: second beam should be horizontal.
    
    %% SCS-I Menuet I, m15
    %% sloped
    %% slope = -0.5ss
    %% concaveness: 0.06
    \clef bass
    \time 3/4
    \key g\major
    a8 g fis e b dis
    
    %% SCS-I Menuet II, m20
    %% horizontal
    %% slope = 0
    %% concaveness: 0.09
    \key f\major
    fis,^"horiz." a c es d c

%%% Sarabande: the first beam, obviously more concave, is not horizontal,
%%% but is matched with the next beam in the piece: context.
    
    %% Sarabande: m24
    %% sloped
    %% concaveness: 0.00
    \stemUp
    [d,16 a' b cis]

    %% Sarabande: m25
    %% horizontal
    %% concaveness:a: 0.12
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

    
%%% Han-Wen: this should be concave
    [a,16^"horiz." a' a a]

    \clef treble

%%%% This should not be concave (hwn)
    [\stemUp bes8  \stemDown d'8 bes8]  
  }
  \paper{
    linewidth = -1.0
  }
}

%% Local variables:
%% LilyPond-indent-level:2
%% End:
