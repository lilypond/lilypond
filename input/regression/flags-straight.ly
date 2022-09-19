\version "2.23.14"

\header {
  texidoc = "Straight flag styles."
}


% test notes, which will be shown in different styles:
testnotes = { \autoBeamOff c'8 d'16 c'32 d'64 \acciaccatura {c'8} d'64
   c''8 d''16 c''32 d''64 \acciaccatura {\stemDown c''8 \stemNeutral} d''64  }

{
  \time 2/4
  \textMark "modern straight"
  \override Flag.stencil = #modern-straight-flag
  \testnotes

  \textMark "old straight (large angles)"
  \override Flag.stencil = #old-straight-flag
  \testnotes

  \textMark "flat"
  \override Flag.stencil = #flat-flag
  \testnotes
%
%   \textMark "custom slant"
% %   Custom straight flag. The parameters are:
% %                flag thickness and spacing
% %                up-flag angle and length
% %               down-flag angle and length
%   \override Flag.stencil = #(straight-flag 0.35 0.8 -5 0.5 60 2.0)
%   \testnotes
}
