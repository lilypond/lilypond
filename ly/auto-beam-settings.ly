%%In the file ly/auto-beam-settings.ly, all necessary i-iii settings should
%%be listed, here's a new version with more comments, (hw, please include).
%%duh

% auto-beam-settings.ly
% setup for auto-beam engraver
%
% specify generic beam end times

% format:
%
%     [time-signature]'beamAutoEnd'[duration]
%
% where
%
%     time-signature = 'time'[numerator]'_'denominator; eg: 3_4
%     duration = [numerator]'_'denominator; eg: 3_8, _16
%

% in 3/2 time:
%   end beams each 1/2 note
%   end beams with 16th notes each 1/4 note
%   end beams with 32th notes each 1/8 note
time3_2beamAutoEnd = "1/2";
time3_2beamAutoEnd_16 = "1/4";
time3_2beamAutoEnd_32 = "1/8";

time3_4beamAutoBegin = "1/4";
time3_4beamAutoEnd_8 = "3/4";
time3_4beamAutoBegin_32 = "1/8";
time3_4beamAutoEnd_32 = "1/8";

time3_8beamAutoBegin = "1/8";
time3_8beamAutoEnd = "3/8";

% in common time:
%   end beams each 1/2 note
%   end beams with 32th notes each 1/8 note
%   end beams with 1/8 triplets each 1/4 note

time4_4beamAutoEnd_8 = "1/2";
time4_4beamAutoEnd_12 = "1/4";
time4_4beamAutoEnd_32 = "1/8";

time4_8beamAutoEnd_8 = "1/4";
time4_8beamAutoEnd_16 = "1/4";
time4_8beamAutoEnd_32 = "1/8";

time4_16beamAutoEnd = "1/8";

time6_8beamAutoEnd_8 = "3/8";
time6_8beamAutoEnd_16 = "3/8";
time6_8beamAutoEnd_32 = "1/8";

time9_8beamAutoEnd_8 = "3/8";
time9_8beamAutoEnd_16 = "3/8";
time6_8beamAutoEnd_32 = "1/8";

%{

Users may override in most cases, simply by issuing

    % from here on consider ending beam every 1/4 note
    \property Voice.beamAutoEnd = "1/4"

    % no autobeaming
    \property Voice.beamAuto = "0"  

or, more globally, by doing:

 \paper{
        \translator{
            \VoiceContext
            % consider ending beam at every 1/2 note
            beamAutoEnd = "1/2";
        }
    }

see also input/test/auto-beam-override.ly

%}

