% params-as.ly
% generic paper parameters

%%paperfile = \papersize + ".ly";
%%% paperfile = "a4.ly";
%%\include \paperfile;
%hsize = 60.0\char;
%vsize = 60.0\char;  %?

%%\include "paper.ly";
linewidth = 60.0\char;
textheight = 60.0\char;
indent = 8.0\char;

staffspace = (\staffheight - 1.0 ) / 4.0;
stafflinethickness = \staffspace / 2.0;

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


% catch suspect beam slopes, set slope to zero if
% outer stem is lengthened more than
beam_lengthened = 0.2 * \staffspace;
% and slope is running away steeper than
beam_steep_slope = 0.2 / 1.0;

%{
dit(code(slur_x_gap)) Horizontal space between note and slur.  Set to
code(\staffspace / 5) by default.  

%}
% OSU: suggested gap = ss / 5;
slur_x_gap = \staffspace / 5.0;
slur_y_gap = 0.25 * \staffspace;
slur_y_free = 0.75 * \staffspace;
slur_x_minimum = 1.5 * \staffspace;


% use tangent controls or area asymmetry?
bezier_area = 1.0;
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

bezier_before_blowfit = 0.5;

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
slur_snap_to_stem = 1.75 * \staffspace;
slur_interstaff_snap_to_stem = 2.5 * \staffspace;
% maximum dy change allowed by snapping
slur_snap_max_slope_change = 0.5;
slur_interstaff_snap_max_slope_change = 0.5;



tie_x_minimum = \staffspace + \slur_x_minimum;
% OSU: tie gap == slur gap
tie_x_gap = \slur_x_gap;
tie_y_gap = 0.25 * \staffspace;
% length of a tie that's a staffspace high
tie_staffspace_length = 4.0 * \staffspace;


%{
 Specifies the maximum height of slurs.
%}
slur_height_limit_factor = \staffheight;


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

% vertical space between lines.
line_kern = \staffspace;


% chop off this much when next to pp / ff sign.
crescendo_shorten = 4.0 * \staffspace;
crescendo_thickness   = \stafflinethickness;
crescendo_height = 0.666 * \staffspace;

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

% Backward compatibility -- has no function; 
Gourlay = 0.0;
Wordwrap =0.0;

\include "engraver.ly";

