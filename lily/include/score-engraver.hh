/*
  score-engraver.hh -- declare Score_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCORE_GRAV_HH
#define SCORE_GRAV_HH

#include "engraver-group-engraver.hh"
#include "global-translator.hh"

/**
  Top level engraver. Puts elements into appropriate columns.
 */
class Score_engraver : 
  public Engraver_group_engraver, public Global_translator 
{
  System * scoreline_l_;
  int breaks_i_;

  Link_array<Grob> elem_p_arr_;
    
  Paper_column* command_column_l_;
  Paper_column* musical_column_l_;
  void make_columns ();
  void set_columns (Paper_column*,Paper_column*);
  void typeset_all ();
    
public:
  TRANSLATOR_DECLARATIONS(Score_engraver);
  Paper_score * pscore_p_;
  
  void forbid_breaks ();

  virtual Music_output *get_output_p ();  
protected:   
  virtual void prepare (Moment);
  virtual void finish ();
  virtual void one_time_step ();
  virtual int depth_i () const { return Global_translator::depth_i ();}

protected:
  /* Engraver_group_engraver interface */
  virtual void acknowledge_grob (Grob_info);

  virtual bool try_music (Music*);
  virtual void initialize ();
  virtual void finalize ();
  virtual void announce_grob (Grob_info);
  virtual void typeset_grob (Grob*elem_p);

  virtual void stop_translation_timestep ();
};

#endif // SCORE_GRAV_HH
