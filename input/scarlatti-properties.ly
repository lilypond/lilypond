


\version "1.3.93";

rh=\property Thread.NoteHead \push #'style = #'default
lh=\property Thread.NoteHead \push #'style = #'diamond

n = \property Thread.NoteHead \pop #'font-size % = #'diamond %\property Thread.fontSize=#0  
sm=\property Thread.NoteHead \push #'font-size = #-1 

% su=\property Voice.verticalDirection=#1  
% sd=\property Voice.verticalDirection=#-1

su=\property Voice.Stem \push #'direction = #1
sd=\property Voice.Stem \push #'direction = #-1  


%{
ls=\property Voice.forceHorizontalShift=#-0.6
sls=\property Voice.forceHorizontalShift=#-0.22
rs=\property Voice.forceHorizontalShift=#0.6
srs=\property Voice.forceHorizontalShift=#0.22
%}
zs = \property Voice.NoteColumn \pop #'force-hshift  % #0.0
ls = \property Voice.NoteColumn \push #'force-hshift = #-0.6
sls= \property Voice.NoteColumn \push #'force-hshift = #-0.22
rs = \property Voice.NoteColumn \push #'force-hshift = #0.6
srs= \property Voice.NoteColumn \push  #'force-hshift = #0.22



ab=\property Voice.noAutoBeaming=##f
xb=\property Voice.noAutoBeaming=##t
