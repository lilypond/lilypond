% params.ly
% generic paper parameters

interline = staffheight / 4.0;
internote = interline / 2.0;
staffline = interline / 10.0;

beam_thickness = 0.48 * (interline - staffline);
interbeam = (2.0 * interline - beam_thickness) / 2.0;
interbeam4 = (3.0 * interline - beam_thickness) / 3.0;

% OSU: suggested gap = ss / 5;
slur_x_gap = interline / 5.0;
slur_x_minimum = 2.0 * interline;
tie_x_minimum = slur_x_minimum;
tie_x_gap = slur_x_gap;

% ugh: rename to bow (in bezier.cc and fonts.doc too...)
slur_thickness = 1.8 * staffline;
slur_height_limit = staffheight;
slur_ratio = 1.0 / 3.0;
slur_clip_ratio = 1.2;
slur_clip_height = 3.0 * staffheight;
slur_clip_angle = 100.0;
slur_rc_factor = 2.4;

% ugh
notewidth = (quartwidth + wholewidth) / 2.0;

% ugh
barsize = staffheight;
rulethickness = staffline;

% mmm
indent = linewidth / 14.0;

% uhm
unitspace = 22.\pt;
geometric = 0.;

gourlay_energybound = 100000.;
%{
The following bounds the number of measures
on a line.  Decreasing it greatly reduces computation time
%}
gourlay_maxmeasures = 10.;
castingalgorithm = \Gourlay;
\include "engraver.ly";

