/*
  score-engraver.hh -- declare Score_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  Line_of_score * scoreline_l_;
  int breaks_i_;

  Link_array<Score_element> elem_p_arr_;
    
  Paper_column* command_column_l_;
  Paper_column* musical_column_l_;
    
  void set_columns (Paper_column*,Paper_column*);
  void typeset_all();
    
public:
  VIRTUAL_COPY_CONS(Translator);
  Paper_score * pscore_p_;
  
  void forbid_breaks ();
  Score_engraver();
  virtual Music_output *get_output_p ();  
protected:   
  virtual void prepare (Moment);
  virtual void finish();
  virtual void process();
  virtual int depth_i() const { return Global_translator::depth_i ();}

protected:
  /* Engraver_group_engraver interface */

  virtual bool do_try_music (Music*);
  virtual void do_creation_processing();
  virtual void do_removal_processing();
  virtual void announce_element (Score_element_info);
  virtual void do_announces();
  virtual void typeset_element (Score_element*elem_p);

  virtual void do_pre_move_processing();
  virtual void do_add_processing ();
};

#endif // SCORE_GRAV_HH
