% params.ly
% generic paper parameters

paperfile = \papersize + ".ly";
% paperfile = "a4.ly";
\include \paperfile;
\include "paper.ly";

interline = \staffheight / 4.0;


stafflinethickness = \interline / 10.0;

% urg, need grace_ versions of these too?
beam_thickness = 0.52 * (\interline - \stafflinethickness);

#'beam-thickness = \beam_thickness;  %% UGR


interbeam = (2.0 * \interline + \stafflinethickness - \beam_thickness) / 2.0;
interbeam4 = (3.0 * \interline - \beam_thickness) / 3.0;

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


#'Stem_tremolo::beam-width = 1.5 * \quartwidth ; 

#'Clef_item::visibility-lambda = #postbreak-only-visibility
#'Key_item::visibility-lambda = #postbreak-only-visibility
#'Breathing_sign::visibility-lambda = #non-postbreak-visibility

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

% used to be 1.4 .
slur_thickness = 1.2 * \stafflinethickness;
tie_thickness = 1.2 * \stafflinethickness;

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

bar_kern = 3.0 * \stafflinethickness;
bar_thinkern = 3.0 * \stafflinethickness;
barthick_thick = 6.0* \stafflinethickness;
barthick_thin = 1.6*\stafflinethickness;
barthick_score = 1.6*\stafflinethickness;

bracket_arch_thick = \interline / 3.0;
bracket_width = 2.0 * \interline;
bracket_thick = 2.0 * \stafflinethickness;
bracket_arch_height = 1.5 * \interline;
bracket_arch_width = \bracket_arch_height;
bracket_arch_angle = 50.0;

tuplet_spanner_gap = 2.0 * \interline;
tuplet_thick = 1.0*\stafflinethickness;
volta_thick = 1.6*\stafflinethickness;
volta_spanner_height = 2.0 *\interline;

% relative thickness of thin lines  1.6 : 1 : 0.8
stemthickness = 0.8*\stafflinethickness;
rulethickness = \stafflinethickness;


extender_height = 0.8*\stafflinethickness;

hyphen_thickness = 0.05*\font_normal;
hyphen_height = 0.2*\font_normal;
hyphen_minimum_length = 0.25*\font_normal;

% Multi-measure rests
multi_measure_rest_x_minimum = 2.5*\staffheight;
multi_measure_rest_padding = 2.0 *\interline;
multi_measure_rest_expand_limit = 10.0;

% chop off this much when next to pp / ff sign.
crescendo_shorten = 4.0 * \interline;
crescendo_thickness   = \stafflinethickness;
crescendo_height = 0.666 * \interline;

% in internote.
restcollision_minimum_dist = 3.0;
restcollision_minimum_beamdist = 1.5;


% unit for note collision resolving
collision_note_width = \notewidth;	%ugh.

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

%{
Relative cost of compressing (vs. stretching).  Increasing this
will cause scores to be set looser
.
%}

compression_energy_factor = 0.6;

% if stem is on middle line, choose this direction.
stem_default_neutral_direction = 1.0;

% in interline
articulation_script_padding_default = 1.0;

% Backward compatibility -- has no function; 
Gourlay = 0.0;
Wordwrap =0.0;

\include "engraver.ly";


#'margin-shape = #'()


% 
#'Local_key_item::left-padding = #'0.2
#'Local_key_item::right-padding = #'0.4

#'Staff_symbol::staff-space = \interline ;
#'Staff_symbol::line-count = #5

