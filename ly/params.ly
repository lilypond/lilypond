% params.ly
% generic paper parameters

#'staff-height = \staffheight;

paperfile = \papersize + ".ly";
% paperfile = "a4.ly";
\include \paperfile;
\include "paper.ly";

staffspace = \staffheight / 4.0;
stafflinethickness = \staffspace / 10.0;

% deprecated
interline = \staffspace;


% urg, need grace_ versions of these too?
beam_thickness = 0.52 * (\staffspace - \stafflinethickness);

#'beam-thickness = \beam_thickness;  %% UGR


interbeam = (2.0 * \staffspace + \stafflinethickness - \beam_thickness) / 2.0;
interbeam4 = (3.0 * \staffspace - \beam_thickness) / 3.0;

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

#'Clef_item::visibility-lambda = #begin-of-line-visible
#'Key_item::visibility-lambda = #begin-of-line-visible
#'Breathing_sign::visibility-lambda = #begin-of-line-invisible

% catch suspect beam slopes, set slope to zero if
% outer stem is lengthened more than
beam_lengthened = 0.2 * \staffspace;
% and slope is running away steeper than
beam_steep_slope = 0.2 / 1.0;



%{
  Slur parameters.
  
  See Documentation/programmer/fonts.doc
%}
% Height-limit (h_inf) = factor * staff_space
slur_height_limit_factor = 2.0;
slur_ratio = 1.0 / 3.0;

slur_thickness = 1.2 * \stafflinethickness;


%{
Horizontal space between centre of notehead and slur.
%}
% OSU: suggested gap = ss / 5;
slur_x_gap = \staffspace / 5.0;
slur_y_gap = 0.25 * \staffspace;
slur_y_free = 0.75 * \staffspace;
slur_x_minimum = 1.5 * \staffspace;


bezier_asymmetry = 1.0;
bezier_beautiful = 2.0;

% The constants that define the valid areas for the middle control points
% Used in de_uglyfy.  Bit empirical.
bezier_control1 = 1.5;
bezier_control2 = 0.8;
bezier_control3 = -2.0;

% URG: the magic constants for area asymmetry
bezier_pct_c0 = -0.2;
bezier_pct_c3 = 0.000006;
bezier_pct_out_max = 0.8;
bezier_pct_in_max = 1.2;
bezier_area_steps = 1.0;

slur_force_blowfit = 0.5;
slur_beautiful = 0.5;


%{
  Tie parameters
%}

tie_height_limit_factor = \slur_height_limit_factor;
tie_ratio = \slur_ratio;
tie_thickness = \slur_thickness;

tie_x_minimum = \staffspace + \slur_x_minimum;
% OSU: tie gap == slur gap
tie_x_gap = \slur_x_gap;
tie_y_gap = 0.25 * \staffspace;
% length of a tie that's a staffspace high
tie_staffspace_length = 4.0 * \staffspace;

tie_staffline_clearance = 2.0 *\tie_thickness;




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

bar_kern = 0.5 * \staffspace;
bar_thinkern = 0.75 * \staffspace;
barthick_thick = 0.5* \staffspace;
barthick_score = 0.13333* \staffspace;
barthick_thin = 0.1*\staffspace;

%}

bar_kern = 3.0 * \stafflinethickness;
bar_thinkern = 3.0 * \stafflinethickness;
barthick_thick = 6.0* \stafflinethickness;
barthick_thin = 1.6*\stafflinethickness;
barthick_score = 1.6*\stafflinethickness;

bracket_arch_thick = \staffspace / 3.0;
bracket_width = 2.0 * \staffspace;
bracket_thick = 2.0 * \stafflinethickness;
bracket_arch_height = 1.5 * \staffspace;
bracket_arch_width = \bracket_arch_height;
bracket_arch_angle = 50.0;

tuplet_spanner_gap = 2.0 * \staffspace;
tuplet_thick = 1.0*\stafflinethickness;
volta_thick = 1.6*\stafflinethickness;
volta_spanner_height = 2.0 *\staffspace;

% relative thickness of thin lines  1.6 : 1 : 0.8
stemthickness = 0.8*\stafflinethickness;
rulethickness = \stafflinethickness;


extender_height = 0.8*\stafflinethickness;

hyphen_thickness = 0.05*\font_normal;
hyphen_height = 0.2*\font_normal;
hyphen_minimum_length = 0.25*\font_normal;

% Multi-measure rests
multi_measure_rest_x_minimum = 2.5*\staffheight;
multi_measure_rest_padding = 2.0 *\staffspace;
multi_measure_rest_expand_limit = 10.0;

% chop off this much when next to pp / ff sign.
crescendo_shorten = 4.0 * \staffspace;
crescendo_thickness   = \stafflinethickness;
crescendo_height = 0.666 * \staffspace;
crescendo_dash_thickness = 1.2*\stafflinethickness;
crescendo_dash = 4.0*\staffspace;

% in internote.
restcollision_minimum_dist = 3.0;
restcollision_minimum_beamdist = 1.5;


% unit for note collision resolving
collision_note_width = \notewidth;	%ugh.

% deprecated!
postBreakPadding = 0.0;

% optical correction amount.
stemSpacingCorrection = 0.5*\staffspace;


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
loose_column_distance = 2.0 * \staffspace;

%{
Relative cost of compressing (vs. stretching).  Increasing this
will cause scores to be set looser
.
%}

compression_energy_factor = 0.6;

% if stem is on middle line, choose this direction.
stem_default_neutral_direction = 1.0;

% in staffspace
articulation_script_padding_default = 1.0;

% Backward compatibility -- has no function; 
Gourlay = 0.0;
Wordwrap =0.0;

\include "engraver.ly";


#'margin-shape = #'()


% 
#'Local_key_item::left-padding = #'0.2
#'Local_key_item::right-padding = #'0.4

#'Staff_symbol::staff-space = \staffspace ;
#'Staff_symbol::line-count = #5

