%  include file for scarlatti-test

\version "1.5.49"

rh=\property Thread.NoteHead \override #'style = #'default
lh=\property Thread.NoteHead \override #'style = #'diamond

n =  \property Thread.NoteHead \revert #'font-relative-size % = #'diamond %\property Thread.fontSize=#0  
sm=\property Thread.NoteHead \override #'font-relative-size = #-1 

% su=\stemUp\slurUp\tieUp  
% sd=\stemDown\slurDown\tieDown

su=\property Voice.Stem \override #'direction = #1
sd=\property Voice.Stem \override #'direction = #-1  


%{
ls=\property Voice.forceHorizontalShift=#-0.6
sls=\property Voice.forceHorizontalShift=#-0.22
rs=\property Voice.forceHorizontalShift=#0.6
srs=\property Voice.forceHorizontalShift=#0.22
%}
zs =  \property Voice.NoteColumn \revert #'force-hshift  % #0.0
ls =  \property Voice.NoteColumn \override #'force-hshift = #-0.6
sls= \property Voice.NoteColumn \override #'force-hshift = #-0.22
rs =  \property Voice.NoteColumn \override #'force-hshift = #0.6
srs= \property Voice.NoteColumn \override  #'force-hshift = #0.22



ab=\property Voice.autoBeaming = ##t
xb=\property Voice.autoBeaming = ##f
