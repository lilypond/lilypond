% params.ly
% generic paper parameters

paperfile = \papersize + ".ly";
\include \paperfile;
\include "paper.ly";

interline = \staffheight / 4.0;
internote = \interline / 2.0;
staffline = \interline / 10.0;

beam_thickness = 0.48 * (\interline - \staffline);
interbeam = (2.0 * \interline - \beam_thickness) / 2.0;
interbeam4 = (3.0 * \interline - \beam_thickness) / 3.0;

% some beam-stemlength settings...
%
%    beam_*1 : multiplicity < beam_multiple_break
%    beam_*2 : multiplicity >= beam_multiple_break
%
beam_multiple_break = 3.0;
beam_minimum_stem1 = 1.5 * \interline;
beam_minimum_stem2 = 1.0 * \interline;
beam_ideal_stem1 = 2.0 * \interline;
beam_ideal_stem2 = 1.5 * \interline;
% beam_slope_damp_correct_factor = 2.0;
beam_slope_damp_correct_factor = 0.0;

% OSU: suggested gap = ss / 5;
slur_x_gap = \interline / 5.0;
slur_x_minimum = 2.0 * \interline;
slur_slope_damping = 0.3;
tie_x_minimum = \slur_x_minimum;
tie_x_gap = \slur_x_gap;
tie_slope_damping = 0.3;

% ugh: rename to bow (in bezier.cc and fonts.doc too...)
% slur_thickness = 1.8 * \staffline;
slur_thickness = 1.4 * \staffline;
slur_height_limit = \staffheight;
% mmm, try bit flatter slurs
% slur_ratio = 1.0 / 3.0;
slur_ratio = 0.3;
slur_clip_ratio = 1.2;
slur_clip_height = 3.0 * \staffheight;
slur_clip_angle = 100.0;
slur_rc_factor = 2.4;

% ugh
notewidth = (\quartwidth + \wholewidth) / 2.0;

% ugh
barsize = \staffheight;
rulethickness = \staffline;

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

