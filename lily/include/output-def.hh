/*
  music-output-def.hh -- declare Output_def

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MUSIC_OUTPUT_DEF_HH
#define MUSIC_OUTPUT_DEF_HH

#include "lily-proto.hh"
#include "virtual-methods.hh"
#include "smobs.hh"
#include "input.hh"

/*
  Output settings for a block of music.

  This devolved into a rather empty class. The distinction between
  various instances is made in the parser, which creates
  midi/layout/paper blocks depending on the keyword read.

  The data structure is set up as recursive: the definitions not
  supplied in layout are looked up in paper. This is done through
  the parent_ field of Output_def. However, such nesting is limited to
  two levels,

  * first because the parser hard-codes the different types
  of output block.

  * Second, because the prime benefit of multiple levels
  (eg. paper containing layout for a score, containing layout of a
  \score embedded in \markup) requires scaling the Stencils (eg. the
  one coming from score at markup level)
  
 */
class Output_def   
{

public:
  VIRTUAL_COPY_CONSTRUCTOR (Output_def, Output_def);
  DECLARE_SMOBS (Output_def);

public:
  SCM scope_;
  Output_def *parent_;
  
  Input input_origin_;
  string user_key_;

  Output_def (Output_def const&);
  Output_def ();

  /*
    variables.
   */
  SCM c_variable (string id) const;
  SCM lookup_variable (SCM sym) const;
  void set_variable (SCM sym, SCM val);
  Real get_dimension (SCM symbol) const;
};
SCM get_font_table (Output_def *def);
void assign_context_def (Output_def *m, SCM transdef);
SCM find_context_def (Output_def const *m, SCM name);

Interval line_dimensions_int (Output_def*def, int);
 

Font_metric *select_encoded_font (Output_def *layout, SCM chain);
Font_metric *select_font (Output_def *layout, SCM chain);

DECLARE_UNSMOB (Output_def, output_def);


Font_metric* find_pango_font (Output_def *layout,  SCM descr, Real factor);
Font_metric *find_scaled_font (Output_def *od, Font_metric *f, Real magnification);
Output_def *scale_output_def (Output_def *def, Real scale);

Real output_scale (Output_def*);

#endif /* MUSIC_OUTPUT_DEF_HH */
