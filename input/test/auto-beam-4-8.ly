\score{
   \notes\relative c''{
     \time 4/8;

%{
    the default for 4/8 (see scm/auto-beam.scm)
     ----  --------
     |  |  |   |--|
    x| x| x|  x| x|
%}
     c8 c c c16 c


%{
    user override
     --------------
     |  |  |   |--|
    x| x| x|  x| x|
%}
     %\property Voice.beamAutoEnd = #(make-moment 2 4)
     \property Voice.autoBeamSettings \push #'(end * * * *) = #(make-moment 2 4)
     c8 c c c16 c

  }
}
    
