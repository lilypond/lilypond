% params.ly
% generic paper parameters

paperfile = \papersize + ".ly";
% paperfile = "a4.ly";
\include \paperfile;
\include "paper.ly";

interline = \staffheight / 4.0;


% thickness of stafflines
staffline = \interline / 10.0;

beam_thickness = 0.52 * (\interline - \staffline);
interbeam = (2.0 * \interline - \beam_thickness) / 2.0;
interbeam4 = (3.0 * \interline - \beam_thickness) / 3.0;

% stems and beams
%
% poor man's array size
stem_max = 3.0;
%
stem_length0 = 3.5*\interline;
stem_length1 = 2.5 * \interline;
stem_length2 = 2.0 * \interline;
stem_length3 = 1.5 * \interline;

% only used for beams
minimum_stem_length0 = 0.0; % not used
minimum_stem_length1 = 1.5 * \interline;
minimum_stem_length2 = 1.25 * \interline;
minimum_stem_length3 = 1.0 * \interline;

% stems in unnatural (forced) direction should be shortened,
% according to [Roush & Gourlay].  Their suggestion to knock off
% a whole staffspace seems a bit drastical: we'll do half.
%
forced_stem_shorten0 = 0.5 * \interline;
forced_stem_shorten1 = \forced_stem_shorten0;
forced_stem_shorten2 = \forced_stem_shorten1;
forced_stem_shorten3 = \forced_stem_shorten2;

% there are several ways to calculate the direction of a beam
% 
% * MAJORITY : number count of up or down notes
% * MEAN     : mean centre distance of all notes
% * MEDIAN   : mean centre distance weighted per note
%
% enum Dir_algorithm { DOWN=-1, UP=1, MAJORITY=2, MEAN, MEDIAN };
%
DOWN = -1.0;
UP = 1.0;
MAJORITY = 2.0;
MEAN = 3.0;
MEDIAN = 4.0;
% [Ross]: majority
beam_dir_algorithm = \MAJORITY;

% catch suspect beam slopes, set slope to zero if
% outer stem is lengthened more than
beam_lengthened = 0.2 * \interline;
% and slope is running away steeper than
beam_steep_slope = 0.2 / 1.0;

% OSU: suggested gap = ss / 5;
slur_x_gap = \interline / 5.0;
slur_x_minimum = 2.0 * \interline;
slur_slope_damping = 0.5;
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
stemthickness = \staffline;


gourlay_energybound = 100000.;
%{
The following bounds the number of measures
on a line.  Decreasing it greatly reduces computation time
%}
gourlay_maxmeasures = 10.;
castingalgorithm = \Gourlay;

%{
Ross. page 151
%}
bar_kern = 0.5 * \interline;
bar_thinkern = 0.75 * \interline;
barthick_thick = 0.5* \interline;
barthick_thin = 0.1*\interline;


\include "engraver.ly";


