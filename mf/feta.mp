% feta.mp

batchmode; 

mode_def ljfour =
 proofing:=0;                   % no, we're not making proofs
 fontmaking:=1;                 % yes, we are making a font
 tracingtitles:=0;              % no, don't show titles at all
 pixels_per_inch:=600;          % that's the meaning of lowres
 blacker:=.25;                  % make pens a bit blacker
 fillin:=.0;                    % compensate for diagonal fillin
 o_correction:=1;               % but don't overshoot as much
 enddef;

if false:
def mode_param (suffix v) (expr e) =
  if unknown mode_guard_.v:
    v := e;
    mode_guard_.v := 1;
  fi
enddef;

% This macro is invoked by all the modes, after |pixels_per_inch|
% has been defined, thus saving some space and time.
def mode_common_setup_ =
  mode_param (proofing, 0);
  mode_param (fontmaking, 1);
  mode_param (tracingtitles, if pixels_per_inch > 700: 1 else: 0 fi);
enddef;

% Works for a 600$\,$dpi Accel-a-Writer {\tt mackay@cs.washington.edu},
% 16 August 95.
mode_def ljfour =                   %\[ HP LaserJet 4 (600dpi)
  mode_param (pixels_per_inch, 600);
  mode_param (blacker, .25);
  mode_param (fillin, 0);
  mode_param (o_correction, 1);
  mode_common_setup_;
enddef;

laserjetfour := ljfour;
fi

mode=ljfour; 
mag=100.0; 

input feta20.mf

