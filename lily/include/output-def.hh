/*
  music-output-def.hh -- declare Output_def

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef MUSIC_OUTPUT_DEF_HH
#define MUSIC_OUTPUT_DEF_HH

#include "string.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"
#include "virtual-methods.hh"
#include "smobs.hh"
#include "input.hh"

/**
  Definition of how to output lilypond.
 */
class Output_def   
{
public:
  VIRTUAL_COPY_CONSTRUCTOR (Output_def, Output_def);
  DECLARE_SMOBS (Output_def,);
public:
  Output_def * parent_;
  Input input_origin_;
  SCM scope_;

  Output_def (Output_def const&);
  Output_def ();
  virtual void derived_mark ();
  
  /*
    variables.
   */
  SCM c_variable (String id) const;
  SCM lookup_variable (SCM sym) const;
  void set_variable (SCM sym, SCM val);
  Real get_dimension (SCM symbol) const;
};

void assign_context_def (Output_def *m, SCM transdef);
SCM find_context_def (Output_def const *m, SCM name);

int get_tempo (Output_def*def, Moment moment);
void set_tempo (Output_def*def, Moment moment, int count_per_minute_i);

Interval line_dimensions_int (Output_def*def, int);
 

Font_metric *select_encoded_font (Output_def *paper, SCM input_encoding, SCM chain);
Font_metric *select_font (Output_def *paper, SCM chain);

DECLARE_UNSMOB (Output_def, output_def);


Font_metric *find_scaled_font (Output_def * od,
			       Font_metric *f, Real m, SCM input_enc_name);
Output_def *scale_output_def (Output_def *def, Real scale);
Real output_scale (Output_def*);

#endif /* MUSIC_OUTPUT_DEF_HH */
