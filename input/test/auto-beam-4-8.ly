\score{
   \notes\relative c''{
     \time 4/8;

%{
    the default for 4/8 (see ly/auto-beam-settings.ly)
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
     \property Voice.beamAutoEnd = "2/4"
     c8 c c c16 c

  }
}
    
