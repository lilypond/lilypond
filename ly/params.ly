% params.ly
% generic paper parameters

paperfile = \papersize + ".ly";
% paperfile = "a4.ly";
\include \paperfile;
\include "paper.ly";

interline = \staffheight / 4.0;


% thickness of stafflines
staffline = \interline / 10.0;

% urg, need grace_ versions of these too?
beam_thickness = 0.52 * (\interline - \staffline);
interbeam = (2.0 * \interline + \staffline - \beam_thickness) / 2.0;
interbeam4 = (3.0 * \interline - \beam_thickness) / 3.0;

% stems and beams
%
% poor man's array size
stem_max = 3.0;

%{ Specify length of stems for notes in the staff
that don't have beams. 
 Measured in staff positions.
%}
stem_length0 = 7.;
stem_length1 = 5.;
stem_length2 = 4.;
stem_length3 = 3.;

%{
The space taken by a note is determined by the formula 

arithmetic_multiplier * ( c + log2 (time) ))

where code(time) is the amount of time a note occupies.  The value
of code(c) is chosen such that the smallest space within a measure is
arithmetic_basicspace.  The smallest space is the one following the
shortest note in the measure.  Typically arithmetic_basicspace is set
to the width of a quarter note head.
%}
arithmetic_basicspace = 2.;
arithmetic_multiplier = 0.9 * \quartwidth ;



% urg.
% if only these ugly arrays were scm,
% we could override them in the Grace context
grace_factor = 0.8;
grace_stem_length0 = \stem_length0 * \grace_factor;
grace_stem_length1 = \stem_length1 * \grace_factor;
grace_stem_length2 = \stem_length2 * \grace_factor;
grace_stem_length3 = \stem_length3 * \grace_factor;

% only used for beams
minimum_stem_length0 = 0.0 ; % not used
minimum_stem_length1 = 3. ;
minimum_stem_length2 = 2.5;
minimum_stem_length3 = 2.0;

grace_minimum_stem_length0 = 0.0 ; % not used
grace_minimum_stem_length1 = \minimum_stem_length1 * \grace_factor;
grace_minimum_stem_length2 = \minimum_stem_length2 * \grace_factor;
grace_minimum_stem_length3 = \minimum_stem_length3 * \grace_factor;

%{
  stems in unnatural (forced) direction should be shortened,
  according to [Roush & Gourlay].  Their suggestion to knock off
  a whole staffspace seems a bit drastical: we'll do half.
%}

forced_stem_shorten0 = 1.0;
forced_stem_shorten1 = \forced_stem_shorten0;
forced_stem_shorten2 = \forced_stem_shorten1;
forced_stem_shorten3 = \forced_stem_shorten2;

% don't shorten grace stems, always up
grace_forced_stem_shorten0 = 0.;
grace_forced_stem_shorten1 = \grace_forced_stem_shorten0;
grace_forced_stem_shorten2 = \grace_forced_stem_shorten1;
grace_forced_stem_shorten3 = \grace_forced_stem_shorten2;

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

%{
dit(code(beam_dir_algorithm)) Specify algorithm for determining
whether beams go up or down.  It is real valued.  If set to 2.0 then
majority selection is used.  If set to 3.0, then mean selection is
used based on the mean center distance.  If set to 4.0 then median
selection is used, based on the median center distance.
%}

% [Ross]: majority
beam_dir_algorithm = \MAJORITY;

% catch suspect beam slopes, set slope to zero if
% outer stem is lengthened more than
beam_lengthened = 0.2 * \interline;
% and slope is running away steeper than
beam_steep_slope = 0.2 / 1.0;

%{
dit(code(slur_x_gap)) Horizontal space between note and slur.  Set to
code(\interline / 5) by default.  

%}
% OSU: suggested gap = ss / 5;
slur_x_gap = \interline / 5.0;
slur_y_gap = 0.25 * \interline;
slur_y_free = 0.75 * \interline;
slur_x_minimum = 1.5 * \interline;

%{
Like beams, slurs often aren't as steep as the notes they encompass.
This sets the amount of damping.
%}
% slope damping: keep dy/dx < slur_slope_damping
slur_slope_damping = 0.3;
slur_interstaff_slope_damping = 0.6;
% height damping: keep h/dx < slur_height_damping
slur_height_damping = 0.4;
slur_interstaff_height_damping = 0.5;
% snap to stem if slur ends closer to stem than
slur_snap_to_stem = 1.75 * \interline;
slur_interstaff_snap_to_stem = 2.5 * \interline;
% maximum dy change allowed by snapping
slur_snap_max_slope_change = 0.5;
slur_interstaff_snap_max_slope_change = 0.5;



tie_x_minimum = \interline + \slur_x_minimum;
% OSU: tie gap == slur gap
tie_x_gap = \slur_x_gap;
tie_y_gap = 0.25 * \interline;
% length of a tie that's a staffspace high
tie_staffspace_length = 4.0 * \interline;

% ugh: rename to bow (in bezier.cc and fonts.doc too...)
% slur_thickness = 1.8 * \staffline;
slur_thickness = 1.4 * \staffline;

%{
 Specifies the maximum height of slurs.
%}
slur_height_limit = \staffheight;


%{
Specifes the ratio of slur hight to slur width
to aim for.  Default value is 0.3. 
%}

% slur_ratio = 0.3;
% try bit flatter slurs
slur_ratio = 0.25;
slur_clip_ratio = 1.2;
slur_clip_height = 3.0 * \staffheight;
slur_clip_angle = 100.0;
slur_rc_factor = 2.4;

% ugh
notewidth = (\quartwidth + \wholewidth) / 2.0;

% ugh
rulethickness = \staffline;

gourlay_energybound = 100000.;
%{
Maximum number of measures per line to try when using Gourlay
method. 
%}
gourlay_maxmeasures = 10.;


%{ Ross. page 151 lists these values, but we think that thick lines
and kernings are too thick.

bar_kern = 0.5 * \interline;
bar_thinkern = 0.75 * \interline;
barthick_thick = 0.5* \interline;
barthick_score = 0.13333* \interline;
barthick_thin = 0.1*\interline;

%}

bar_kern = 3.0 * \staffline;
bar_thinkern = 3.0 * \staffline;
barthick_thick = 6.0* \staffline;
barthick_thin = 1.6*\staffline;
barthick_score = 1.6*\staffline;

tuplet_spanner_gap = 2.0 * \interline;
tuplet_thick = 1.0*\staffline;
volta_thick = 1.6*\staffline;
volta_spanner_height = 2.0 *\interline;

% relative thickness of thin lines  1.6 : 1 : 0.8
stemthickness = 0.8*\staffline;
rulethickness = \staffline;


extender_height = 0.8*\staffline;

hyphen_thickness = 0.05*\font_normal;
hyphen_height = 0.2*\font_normal;
hyphen_minimum_length = 0.25*\font_normal;

% Multi-measure rests
mmrest_x_minimum = 1.4*\staffheight;


% chop off this much when next to pp / ff sign.
crescendo_shorten = 4.0 * \interline;
crescendo_thickness   = \staffline;

% in internote.
restcollision_minimum_dist = 3.0;
restcollision_minimum_beamdist = 1.5;

% deprecated!
postBreakPadding = 0.0;

% optical correction amount.
stemSpacingCorrection = 0.5*\interline;


%{
 relative strength of space following breakable columns (eg. prefatory matter)
 %}
breakable_column_space_strength = 2.0; 

% space after inline clefs and such get this much stretched
decrease_nonmus_spacing_factor = 1.0 ;

%{
 space before musical columns (eg. taken by accidentals) get this much
 stretched when they follow a musical column, in absence of grace notes.

 0.0 means no extra space (accidentals are ignored)
%}
musical_to_musical_left_spacing_factor = 0.4;

%{
 stretch space this much if there are grace notes before the column
%}
before_grace_spacing_factor = 1.2;

%{
If columns do not have spacing information set, set it to this much
%}
loose_column_distance = 2.0 * \interline;

% if stem is on middle line, choose this direction.
stem_default_neutral_direction = 1.0;

% in interline
articulation_script_padding_default = 1.0;

% Backward compatibility -- has no function; 
Gourlay = 0.0;
Wordwrap =0.0;

\include "engraver.ly";



