% params.ly
% generic paper parameters

%{

TODO:

* cleanup
* use elt properties, iso. paper variables.
%}



paperfile = \papersize + ".ly";
% paperfile = "a4.ly";
\include \paperfile;
\include "paper.ly";

staffspace = \staffheight / 4.0;
stafflinethickness = \staffspace / 10.0;

% deprecated
interline = \staffspace;

%{
The space taken by a note is determined by the formula 

   SPACE = arithmetic_multiplier * ( C + log2 (TIME) ))

where TIME is the amount of time a note occupies.  The value of C is
chosen such that the smallest space within a measure is
arithmetic_basicspace:

  C = arithmetic_basicspace - log2 (mininum (SHORTEST, 1/8)) 

The smallest space is the one following the shortest note in the
measure, or the space following a hypothetical 1/8 note.  Typically
arithmetic_basicspace is set to a value so that the shortest note
takes about two noteheads of space (ie, is followed by a notehead of
space):

   2*quartwidth = arithmetic_multiplier * ( C + log2 (SHORTEST) ))

   { using: C = arithmetic_basicspace - log2 (mininum (SHORTEST, 1/8)) }
   { assuming: SHORTEST <= 1/8 }

               = arithmetic_multiplier *
	       ( arithmetic_basicspace - log2 (SHORTEST) + log2 (SHORTEST) )

               = arithmetic_multiplier * arithmetic_basicspace

   { choose: arithmetic_multiplier = 1.0*quartwidth (why?)}

               = quartwidth * arithmetic_basicspace

   =>	       

   arithmetic_basicspace = 2/1 = 2

If you want to space your music wider, use something like:

   arithmetic_basicspace = 4.;

%}
% We use 0.9*\quartwidth, because 1.0 seems to wide.
% quartwidth == 1.32 * staffspace
% We don't adjust arithmetic_basicspace accordingly (why not?)
arithmetic_multiplier = 0.9 * 1.32 * \staffspace ;
arithmetic_basicspace = 2.0;


% 
% UGH; junk these!
%

% catch suspect beam slopes, set slope to zero if
% outer stem is lengthened more than
beam_lengthened = 0.2 * \staffspace;
% and slope is running away steeper than
beam_steep_slope = 0.2 / 1.0;



%{
  Slur parameters.
  
  See Documentation/programmer/fonts.doc

  TODO: -> elt-properties.
%}
% Height-limit (h_inf) = factor * staff_space
slur_height_limit_factor = 2.0;
slur_ratio = 1.0 / 3.0;

slur_thickness = 1.2 * \stafflinethickness;

slur_force_blowfit = 0.5;
slur_beautiful = 0.5;


%{
Horizontal space between centre of notehead and slur.
%}
% OSU: suggested gap = ss / 5;
slur_x_gap = \staffspace / 5.0;
slur_y_gap = 0.25 * \staffspace;
slur_y_free = 0.75 * \staffspace;
slur_x_minimum = 1.5 * \staffspace;

% URG: the magic constants for area asymmetry
bezier_pct_c0 = -0.2;
bezier_pct_c3 = 0.000006;
bezier_pct_out_max = 0.8;
bezier_pct_in_max = 1.2;
bezier_area_steps = 1.0;


%{
  Tie parameters
%}

tie_height_limit_factor = 1.0 ; 
tie_ratio = \slur_ratio;

% OSU: tie gap == slur gap
tie_x_gap = 0.2 * \staffspace;
tie_staffline_clearance = 2.0 *\slur_thickness;

% vertical space between lines of text.
line_kern = \staffspace;

% chop off this much when next to pp / ff sign.
crescendo_shorten = 4.0 * \staffspace;
crescendo_thickness   = \stafflinethickness;
crescendo_height = 0.666 * \staffspace;
crescendo_dash_thickness = 1.2*\stafflinethickness;
crescendo_dash = 4.0*\staffspace;


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



\include "engraver.ly";





