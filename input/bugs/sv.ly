
%
% Jan ->  dit is Auto_B_E bug, niet grouping bug.
% 

\score{
  \notes{ \time 13/8;
 %    \property Voice.beamAutoBegin = "1/8"
 %         \property Voice.beamAutoEnd = "3/8"
    \property Voice.beamAutoEnd = "10/8"
    \property Voice."beamAutoEnd_16" = "1/8"
    \property Voice.beamAutoBegin = "1/4"

     a8 a a a16 a16 a16 a16 a16 a16 a8 a a a a a
     a a a a a a a a a a a a a
     a8 a8 a8 a8 a8 a a a a a a a a
   }
   \paper{ linewidth = -1.;}
}

