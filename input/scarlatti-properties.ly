


\version "1.3.70";

rh=\property Thread.noteHeadStyle=##f 
lh=\property Thread.noteHeadStyle = #'diamond
n=\property Thread.fontSize=#0  
sm=\property Thread.fontSize=#-1 
% su=\property Voice.verticalDirection=#1  
% sd=\property Voice.verticalDirection=#-1

su=\property Voice.basicStemProperties \push #'direction = #1
sd=\property Voice.basicStemProperties \push #'direction = #-1  

zs=\property Voice.forceHorizontalShift=#0.0
ls=\property Voice.forceHorizontalShift=#-0.6
sls=\property Voice.forceHorizontalShift=#-0.22
rs=\property Voice.forceHorizontalShift=#0.6
srs=\property Voice.forceHorizontalShift=#0.22
ab=\property Voice.noAutoBeaming=##f
xb=\property Voice.noAutoBeaming=##t
